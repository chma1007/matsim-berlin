package org.matsim.run;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.geotools.api.feature.simple.SimpleFeature;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.prep.PreparedGeometry;
import org.locationtech.jts.geom.prep.PreparedGeometryFactory;

import org.matsim.api.core.v01.events.PersonMoneyEvent;
import org.matsim.api.core.v01.events.handler.PersonMoneyEventHandler;
import org.matsim.api.core.v01.network.Link;

import org.matsim.analysis.personMoney.PersonMoneyEventsAnalysisModule;
import org.matsim.contrib.parking.parkingcost.config.ParkingCostConfigGroup;
import org.matsim.contrib.parking.parkingcost.module.ParkingCostModule;

import org.matsim.core.config.Config;
import org.matsim.core.controler.AbstractModule;
import org.matsim.core.controler.Controler;
import org.matsim.core.controler.events.ShutdownEvent;
import org.matsim.core.controler.events.StartupEvent;
import org.matsim.core.controler.listener.ShutdownListener;
import org.matsim.core.controler.listener.StartupListener;

import org.matsim.core.utils.gis.ShapeFileReader;
import org.matsim.core.utils.geometry.geotools.MGC;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

/**
 * Hundekopf 内分区差异化停车费（€/h）。
 * - 输入：EPSG:25832 的多要素 shapefile（每个 feature=子区），属性列含区名（如 SCHLUESSEL）
 * - 输出：为每条 car link 写入：
 *      pc_car = 该区费率（€/h）   ← ParkingCostModule 使用
 *      pc_zone = 该区名字        ← 仅用于检查/可视化
 */
public class RunOpenBerlinWithParking extends OpenBerlinScenario {

	private static final Logger log = LogManager.getLogger(RunOpenBerlinWithParking.class);

	/** 分区后的 Hundekopf 面数据（EPSG:25832） */
	private static final String HUNDEKOPF_ZONES_SHP =
		"D:/berlin/input/v6.4/berlin hundekopf/hundekopf_zones_25832.shp";

	/** shapefile 中表示区名的字段名（你已把 SCHLUESSEL 改成区名了） */
	private static final String ZONE_NAME_ATTR = "SCHLUESSEL";

	/** 分区费率表（示例，单位 €/h），名字用你属性表中的小写写法 */
	private static final Map<String, Double> RATE_BY_ZONE = new HashMap<>() {{
		put("mitte", 4.0);
		put("pankow", 3.0);
		put("neukölln", 2.5);
		put("charlottenburg-willmersdorf", 3.5);
		put("friedrichshain-kreuzberg", 3.5);
		put("treptow-köpenick", 3.0);
		put("tempelhof-schöneberg", 3.0);
		// …其余子区按需补齐；未匹配的用 DEFAULT_RATE
	}};

	private static final double DEFAULT_RATE = 2.5; // 未匹配区名时的兜底费率（可改或改为抛错）

	public static void main(String[] args) {
		run(RunOpenBerlinWithParking.class, args);
	}

	// ---------- Config ----------
	@Override
	protected Config prepareConfig(Config config) {
		config.addModule(new ParkingCostConfigGroup());
		prepareParkingCostConfig(config);
		return super.prepareConfig(config);
	}

	private void prepareParkingCostConfig(Config config) {
		config.setParam("parkingCosts", "useParkingCost", "true");
		config.setParam("parkingCosts", "linkAttributePrefix", "pc_"); // => 读取 pc_car
		config.setParam("parkingCosts", "modesWithParkingCosts", "car");
		config.setParam("parkingCosts", "activityTypesWithoutParkingCost", "home,freight");
	}

	// ---------- Controler ----------
	@Override
	protected void prepareControler(Controler controler) {
		super.prepareControler(controler);

		// 1) 读取多要素 shapefile：每个 feature 一个分区
		List<ZoneDef> zones = loadZones(HUNDEKOPF_ZONES_SHP, ZONE_NAME_ATTR);
		log.info("Loaded {} Hundekopf sub-zones (CRS assumed EPSG:25832).", zones.size());

		// 2) 遍历网络：计算link中点，判断落在哪个分区；写入 pc_car（费率）和 pc_zone（区名）
		int affected = 0, skipped = 0;
		double tolledKm = 0.0;

		for (Link link : controler.getScenario().getNetwork().getLinks().values()) {
			if (!link.getAllowedModes().contains("car")) { skipped++; continue; }

			double mx = 0.5 * (link.getFromNode().getCoord().getX() + link.getToNode().getCoord().getX());
			double my = 0.5 * (link.getFromNode().getCoord().getY() + link.getToNode().getCoord().getY());
			Point mid = MGC.xy2Point(mx, my); // 坐标已是 25832

			String zoneName = null;
			for (ZoneDef z : zones) {
				if (z.geom.covers(mid)) { zoneName = z.name; break; }
			}
			if (zoneName == null) { skipped++; continue; }

			double rate = RATE_BY_ZONE.getOrDefault(zoneName, DEFAULT_RATE);

			// 关键：ParkingCostModule 读取的是 pc_car
			link.getAttributes().putAttribute("pc_car", rate);

			// 仅用于检查/可视化
			link.getAttributes().putAttribute("pc_zone", zoneName);

			affected++;
			tolledKm += link.getLength() / 1000.0;
		}

		log.info("Parking cost tagging by zone done. affectedLinks={}, skippedLinks={}, tolledLen={}.",
			affected, skipped, String.format("%.1f km", tolledKm));

		if (affected == 0) {
			log.warn("No links tagged. Check CRS=EPSG:25832 and that zones overlap the network.");
		}

		// 3) 启用停车计费与人均收支分析
		controler.addOverridingModule(new ParkingCostModule());
		controler.addOverridingModule(new PersonMoneyEventsAnalysisModule());

		// 4) 记录 PersonMoneyEvent（可选）
		String outputDir = controler.getConfig().controller().getOutputDirectory();
		ParkingCostTracker tracker = new ParkingCostTracker(outputDir);
		controler.addOverridingModule(new AbstractModule() {
			@Override public void install() {
				addEventHandlerBinding().toInstance(tracker);
				addControlerListenerBinding().toInstance(tracker);
			}
		});
	}

	// ---------- 读取多区 shapefile ----------
	private static List<ZoneDef> loadZones(String shpPath, String attrName) {
		List<ZoneDef> zones = new ArrayList<>();
		for (SimpleFeature f : ShapeFileReader.getAllFeatures(shpPath)) {
			Object attr = f.getAttribute(attrName);
			String name = (attr == null) ? null : attr.toString().trim().toLowerCase();
			Geometry g = (Geometry) f.getDefaultGeometry();
			if (name == null || name.isEmpty() || g == null || g.isEmpty()) continue;
			zones.add(new ZoneDef(name, PreparedGeometryFactory.prepare(g)));
		}
		if (zones.isEmpty()) {
			throw new IllegalStateException("No valid zone features read from: " + shpPath +
				" (attr=" + attrName + ")");
		}
		return zones;
	}

	private record ZoneDef(String name, PreparedGeometry geom) { }

	// ---------- 事件跟踪（沿用你的实现） ----------
	static class ParkingCostTracker implements PersonMoneyEventHandler, StartupListener, ShutdownListener {
		private PrintWriter writer;
		private final String outputDir;
		private double totalCost = 0.0;

		public ParkingCostTracker(String outputDir) { this.outputDir = outputDir; }

		@Override
		public void notifyStartup(StartupEvent event) {
			try {
				writer = new PrintWriter(new FileWriter(outputDir + "/personCostEvents.tsv"));
				writer.println("time\tpersonId\tamount\ttype");
			} catch (IOException e) {
				throw new RuntimeException("无法写入 personCostEvents.tsv", e);
			}
		}

		@Override
		public void handleEvent(PersonMoneyEvent event) {
			if (event.getPurpose() != null && event.getPurpose().endsWith("parking cost")) {
				double cost = -event.getAmount(); // 支出为负，这里取正
				totalCost += cost;
				writer.printf("%.1f\t%s\t%.2f\t%s%n",
					event.getTime(),
					event.getPersonId(),
					cost,
					event.getPurpose());
			}
		}

		@Override public void reset(int iteration) { /* 不清空累计 */ }

		@Override
		public void notifyShutdown(ShutdownEvent event) {
			if (writer != null) writer.close();
			log.info("Total parking cost charged: {} €", String.format("%.2f", totalCost));
		}
	}
}

