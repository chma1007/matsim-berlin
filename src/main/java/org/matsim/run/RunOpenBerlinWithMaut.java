package org.matsim.run;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.prep.PreparedGeometry;
import org.locationtech.jts.geom.prep.PreparedGeometryFactory;

import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.api.core.v01.events.PersonMoneyEvent;
import org.matsim.api.core.v01.events.handler.PersonMoneyEventHandler;
import org.matsim.contrib.roadpricing.*;
import org.matsim.analysis.personMoney.PersonMoneyEventsAnalysisModule;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.controler.AbstractModule;
import org.matsim.core.controler.Controler;
import org.matsim.core.controler.events.StartupEvent;
import org.matsim.core.controler.events.ShutdownEvent;
import org.matsim.core.controler.listener.StartupListener;
import org.matsim.core.controler.listener.ShutdownListener;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.core.utils.geometry.geotools.MGC;
import org.matsim.core.utils.io.IOUtils;
import org.matsim.utils.gis.shp2matsim.ShpGeometryUtils;

import java.net.URL;
import java.nio.file.*;
import java.io.*;
import java.util.*;

/**
 * 简化版 Maut：Hundekopf 区域内所有 car link 都按距离收费（每 km 2.5 €）。
 * 无时间段、无折扣、无道路类型筛选。
 * 同时给收费 link 打上属性 toll_car=true，方便可视化与对比。
 */
public class RunOpenBerlinWithMaut extends OpenBerlinScenario {

	private static final Logger log = LogManager.getLogger(RunOpenBerlinWithMaut.class);

	// === 路径 ===
	private static final String HUNDEKOPF_SHP = "D:/berlin/input/v6.4/berlin hundekopf/hundekopf_zones_25832.shp";
	private static final String RP_FILENAME = "maut_distance_allLinks.xml";

	// === 费率 ===
	private static final double EUR_PER_M = 2.5 / 1000.0;  // 每米 0.0025 €

	public static void main(String[] args) { run(RunOpenBerlinWithMaut.class, args); }

	@Override
	protected Config prepareConfig(Config config) {
		ConfigUtils.addOrGetModule(config, RoadPricingConfigGroup.class);
		return super.prepareConfig(config);
	}

	@Override
	protected void prepareControler(Controler controler) {
		super.prepareControler(controler);

		// 1) 读取 Hundekopf 区域
		PreparedGeometry zone = loadUnion(HUNDEKOPF_SHP);
		log.info("Hundekopf zone loaded.");

		// 2) 选择 Hundekopf 内的所有 car link
		Set<Id<Link>> tolledLinks = selectLinksInsideZone(controler.getScenario().getNetwork(), zone);
		log.info("Links inside Hundekopf zone: {}", tolledLinks.size());

		// === 新增：打上 toll_car 属性 ===
		for (Id<Link> id : tolledLinks) {
			Link link = controler.getScenario().getNetwork().getLinks().get(id);
			link.getAttributes().putAttribute("toll_car", true);
		}
		log.info("Marked {} links with attribute toll_car=true", tolledLinks.size());

		// 3) 生成 roadpricing XML 文件
		String outDir = controler.getConfig().controller().getOutputDirectory();
		URL rpOutUrl = IOUtils.extendUrl(controler.getConfig().getContext(), outDir + "/" + RP_FILENAME);
		Path rpPath;
		try {
			rpPath = Paths.get(rpOutUrl.toURI());
			Files.createDirectories(rpPath.getParent());
		} catch (Exception ex) {
			throw new RuntimeException("Cannot prepare output path for roadpricing xml: " + rpOutUrl, ex);
		}
		writeSimpleDistanceScheme(tolledLinks, EUR_PER_M, rpPath.toString());
		log.info("Roadpricing XML written: {}", rpPath);

		// 4) 配置 RoadPricing 模块
		RoadPricingConfigGroup rpCfg = ConfigUtils.addOrGetModule(controler.getConfig(), RoadPricingConfigGroup.class);
		rpCfg.setTollLinksFile(rpPath.toString());
		controler.addOverridingModule(new RoadPricingModule());

		// 5) 输出 PersonMoneyEvents TSV
		RoadPricingEventsLogger logger = new RoadPricingEventsLogger();
		controler.addOverridingModule(new AbstractModule() {
			@Override public void install() {
				addEventHandlerBinding().toInstance(logger);
				addControlerListenerBinding().toInstance(logger);
				install(new PersonMoneyEventsAnalysisModule());
			}
		});
	}

	// === Helper 方法 ===

	/** 合并 shapefile 区域为单一多边形。 */
	private static PreparedGeometry loadUnion(String shpPath) {
		URL url = IOUtils.resolveFileOrResource(shpPath);
		var preparedParts = ShpGeometryUtils.loadPreparedGeometries(url);
		List<Geometry> geoms = new ArrayList<>();
		preparedParts.forEach(pg -> geoms.add(pg.getGeometry()));
		Geometry union = org.locationtech.jts.operation.union.UnaryUnionOp.union(geoms);
		return PreparedGeometryFactory.prepare(union);
	}

	/** 选择所有 Hundekopf 内的 car link。 */
	private static Set<Id<Link>> selectLinksInsideZone(Network net, PreparedGeometry zone) {
		Set<Id<Link>> ids = new HashSet<>();
		for (Link link : net.getLinks().values()) {
			if (!link.getAllowedModes().contains("car")) continue;
			var from = link.getFromNode().getCoord();
			var to = link.getToNode().getCoord();
			double mx = 0.5 * (from.getX() + to.getX());
			double my = 0.5 * (from.getY() + to.getY());
			if (zone.covers(MGC.xy2Point(mx, my))) ids.add(link.getId());
		}
		return ids;
	}

	/** 生成简单的 distance-based 收费方案（全天相同费率）。 */
	private static void writeSimpleDistanceScheme(Set<Id<Link>> linkIds, double eurPerMeter, String outFile) {
		var tmp = ScenarioUtils.createScenario(ConfigUtils.createConfig());
		RoadPricingSchemeImpl scheme = RoadPricingUtils.addOrGetMutableRoadPricingScheme(tmp);
		RoadPricingUtils.setType(scheme, RoadPricingScheme.TOLL_TYPE_DISTANCE);
		RoadPricingUtils.setName(scheme, "Hundekopf_distance_flat");
		RoadPricingUtils.setDescription(scheme, "All car links inside Hundekopf tolled by distance (flat rate)");
		linkIds.forEach(id -> RoadPricingUtils.addLink(scheme, id));
		RoadPricingUtils.createAndAddGeneralCost(scheme, 0.0, 24*3600, eurPerMeter);
		new RoadPricingWriterXMLv1(scheme).writeFile(outFile);
	}

	/** 简单事件记录器，输出 personCostEvents.tsv（与停车方案一致） */
	static class RoadPricingEventsLogger implements PersonMoneyEventHandler, StartupListener, ShutdownListener {
		private PrintWriter out;
		private Path tsvPath;
		private double total = 0.0;

		@Override public void notifyStartup(StartupEvent e) {
			try {
				String outDir = e.getServices().getConfig().controller().getOutputDirectory();
				tsvPath = Paths.get(outDir, "personCostEvents.tsv");
				Files.createDirectories(tsvPath.getParent());
				out = new PrintWriter(new FileWriter(tsvPath.toFile()));
				out.println("time\tpersonId\tamount\ttype");
			} catch (IOException ex) {
				throw new RuntimeException("Cannot open TSV for road pricing events", ex);
			}
		}

		@Override public void handleEvent(PersonMoneyEvent ev) {
			double paid = -ev.getAmount(); // 支出→正值
			synchronized (this) {
				out.printf(Locale.ROOT, "%.1f\t%s\t%.2f\t%s%n",
					ev.getTime(), ev.getPersonId(), paid, ev.getPurpose());
			}
			total += paid;
		}

		@Override public void reset(int iteration) { }
		@Override public void notifyShutdown(ShutdownEvent e) {
			if (out != null) {
				out.flush();
				out.close();
			}
			log.info("Total road pricing revenue: {} €", String.format(Locale.ROOT,"%.2f", total));
		}
	}
}
