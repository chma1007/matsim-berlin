package org.matsim.run;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.prep.PreparedGeometry;
import org.locationtech.jts.geom.prep.PreparedGeometryFactory;

import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.population.Activity;
import org.matsim.api.core.v01.population.Person;
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
import org.matsim.core.utils.misc.Time;

import org.matsim.utils.gis.shp2matsim.ShpGeometryUtils;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

/** 主线方案：狗头内按公里收费；只收干道；峰时(07–10,16–19)；居民自动 50%（TollFactor）。
 *  同时输出与停车方案同结构的 TSV（personCostEvents.tsv）。
 */
public class RunOpenBerlinWithMaut extends OpenBerlinScenario {

	private static final Logger log = LogManager.getLogger(RunOpenBerlinWithMaut.class);

	// === 路径（SHP 必须 EPSG:25832）===
	private static final String HUNDEKOPF_ZONES_SHP =
		"D:/berlin/input/v6.4/berlin hundekopf/hundekopf_zones_25832.shp";
	private static final String RP_FILENAME = "maut_distance_peak_arterials.generated.xml";

	// 价率：2.5 €/km = 0.0025 €/m
	private static final double EUR_PER_M = 2.5 / 1000.0;

	// 峰时窗 [start,end)（Time.parseTime 返回 double 秒）
	private static final double[][] PEAK_WINDOWS = new double[][]{
		{ Time.parseTime("07:00:00"), Time.parseTime("10:00:00") },
		{ Time.parseTime("16:00:00"), Time.parseTime("19:00:00") }
	};

	private static final String HOME_PREFIX = "home"; // 识别居民：home* 活动在狗头内

	public static void main(String[] args) { run(RunOpenBerlinWithMaut.class, args); }

	@Override protected Config prepareConfig(Config config) {
		ConfigUtils.addOrGetModule(config, RoadPricingConfigGroup.class);
		return super.prepareConfig(config);
	}

	@Override protected void prepareControler(Controler controler) {
		super.prepareControler(controler);

		// 1) 读狗头多边形（稳健：先解析 URL，再读，再 union）
		PreparedGeometry zone = loadUnion(HUNDEKOPF_ZONES_SHP);
		log.info("Hundekopf zone loaded (EPSG:25832).");

		// 2) 选择：狗头内 + 干道 + car 的 link
		Set<Id<Link>> tolled = selectArterialsInsideZone(controler.getScenario().getNetwork(), zone);
		log.info("Arterials inside Hundekopf selected: {}", tolled.size());

		// 3) 生成 roadpricing XML 到输出目录（先确保目录存在）
		String outDir = controler.getConfig().controller().getOutputDirectory();
		URL rpOutUrl = IOUtils.extendUrl(controler.getConfig().getContext(), outDir + "/" + RP_FILENAME);
		Path rpPath;
		try {
			rpPath = Paths.get(rpOutUrl.toURI());
			Files.createDirectories(rpPath.getParent());
		} catch (Exception ex) {
			throw new RuntimeException("Cannot prepare output path for roadpricing xml: " + rpOutUrl, ex);
		}
		writeDistanceWithPeakCosts(tolled, EUR_PER_M, PEAK_WINDOWS, rpPath.toString());
		log.info("Roadpricing XML written: {}", rpPath);

		// 4) 回填到 config（VIA 也可加载这份文件）
		RoadPricingConfigGroup rpCfg = ConfigUtils.addOrGetModule(controler.getConfig(), RoadPricingConfigGroup.class);
		rpCfg.setTollLinksFile(rpPath.toString());

		// 5) 识别“居民”（selected plan 的第一个 home* 在狗头内）
		Set<Id<Person>> residents = detectResidentsInsideZone(controler, zone);
		log.info("Residents inside zone: {}", residents.size());

		// 6) TollFactor：居民 0.5×；其他 1.0×
		TollFactor tf = (personId, vehicleId, linkId, time) -> residents.contains(personId) ? 0.5 : 1.0;

		// 7) 用同一个 URL 注入 TollFactor 版 scheme（不要再 RoadPricing.configure）
		RoadPricingSchemeUsingTollFactor scheme =
			RoadPricingSchemeUsingTollFactor.createAndRegisterRoadPricingSchemeUsingTollFactor(
				rpOutUrl, tf, controler.getScenario());
		controler.addOverridingModule(new RoadPricingModule(scheme));

		// 8) 绑定 road pricing 事件记录器（注意：既是 EventHandler 也是 ControlerListener）
		RoadPricingEventsLogger rpLogger = new RoadPricingEventsLogger(residents);
		controler.addOverridingModule(new AbstractModule() {
			@Override public void install() {
				addEventHandlerBinding().toInstance(rpLogger);
				addControlerListenerBinding().toInstance(rpLogger); // ★ 必须加，保证 notifyStartup 被调用
				install(new PersonMoneyEventsAnalysisModule());
			}
		});
	}

	// ===== helpers =====

	/** 稳健读取 shapefile：解析 URL → 读取 PreparedGeometry → union 成单个 PreparedGeometry。 */
	private static PreparedGeometry loadUnion(String shpPath) {
		URL url = IOUtils.resolveFileOrResource(shpPath);
		if (url == null) throw new IllegalArgumentException("Shapefile not found: " + shpPath);

		var preparedParts = ShpGeometryUtils.loadPreparedGeometries(url);
		if (preparedParts.isEmpty()) throw new IllegalStateException("No polygon geometry in: " + shpPath);

		List<Geometry> geoms = new ArrayList<>();
		preparedParts.forEach(pg -> geoms.add(pg.getGeometry()));

		Geometry union = org.locationtech.jts.operation.union.UnaryUnionOp.union(geoms);
		return PreparedGeometryFactory.prepare(union);
	}

	/** 选择：狗头内 + 干道 + car 的所有 link。 */
	private static Set<Id<Link>> selectArterialsInsideZone(Network net, PreparedGeometry zone) {
		Set<Id<Link>> s = new HashSet<>();
		for (Link link : net.getLinks().values()) {
			if (!link.getAllowedModes().contains("car")) continue;
			if (!isArterial(link)) continue;

			var from = link.getFromNode().getCoord();
			var to   = link.getToNode().getCoord();
			double mx = 0.5 * (from.getX() + to.getX());
			double my = 0.5 * (from.getY() + to.getY());
			if (zone.covers(MGC.xy2Point(mx, my))) s.add(link.getId());
		}
		return s;
	}

	/** OSM 高等路优先；兜底：freespeed≥50km/h 或 lanes≥2。必要时改字段名或阈值。 */
	private static boolean isArterial(Link link) {
		Object hwy =
			link.getAttributes().getAttribute("highway") != null ? link.getAttributes().getAttribute("highway") :
				link.getAttributes().getAttribute("type")    != null ? link.getAttributes().getAttribute("type") :
					link.getAttributes().getAttribute("osm:way:highway");
		if (hwy != null) {
			String v = hwy.toString().toLowerCase(Locale.ROOT);
			if (v.contains("primary") || v.contains("secondary") || v.contains("tertiary") || v.contains("trunk"))
				return true;
		}
		double vFree = link.getFreespeed(); // m/s
		double lanes = 1.0;
		Object lanesAttr = link.getAttributes().getAttribute("lanes");
		if (lanesAttr instanceof Number) lanes = ((Number) lanesAttr).doubleValue();
		else if (lanesAttr != null) try { lanes = Double.parseDouble(lanesAttr.toString()); } catch (Exception ignore) {}
		return (vFree >= 13.9) || (lanes >= 2.0);
	}

	/** 写 distance 方案：仅峰时收费；其他时段免费。 */
	private static void writeDistanceWithPeakCosts(Set<Id<Link>> linkIds, double eurPerMeter, double[][] peakWindows, String outFile) {
		var tmp = ScenarioUtils.createScenario(ConfigUtils.createConfig());
		RoadPricingSchemeImpl scheme = RoadPricingUtils.addOrGetMutableRoadPricingScheme(tmp);
		RoadPricingUtils.setType(scheme, RoadPricingScheme.TOLL_TYPE_DISTANCE);
		RoadPricingUtils.setName(scheme, "Hundekopf_distance_peak_only_arterials");
		RoadPricingUtils.setDescription(scheme, "Arterials inside Hundekopf tolled by distance (peak only)");
		linkIds.forEach(id -> RoadPricingUtils.addLink(scheme, id));
		for (double[] w : peakWindows) {
			RoadPricingUtils.createAndAddGeneralCost(scheme, w[0], w[1], eurPerMeter);
		}
		new RoadPricingWriterXMLv1(scheme).writeFile(outFile);
	}

	/** 识别“居民”：selected plan 的第一个 home* 活动在狗头内。 */
	private static Set<Id<Person>> detectResidentsInsideZone(Controler controler, PreparedGeometry zone){
		Set<Id<Person>> residents = new HashSet<>();
		controler.getScenario().getPopulation().getPersons().values().forEach(p -> {
			var plan = p.getSelectedPlan();
			if (plan == null) return;
			plan.getPlanElements().stream()
				.filter(pe -> pe instanceof Activity)
				.map(pe -> (Activity) pe)
				.filter(act -> act.getType()!=null && act.getType().startsWith(HOME_PREFIX))
				.findFirst()
				.ifPresent(act -> {
					var c = act.getCoord();
					if (c!=null && zone.covers(MGC.xy2Point(c.getX(), c.getY()))) {
						residents.add(p.getId());
					}
				});
		});
		return residents;
	}

	// ============ 事件记录器（TSV，与停车方案同结构） ============

	/** 抓 road pricing 的 PersonMoneyEvent，写 TSV：time personId amount type（amount 为正，单位€）。 */
	static class RoadPricingEventsLogger implements PersonMoneyEventHandler, StartupListener, ShutdownListener {
		private final Set<Id<Person>> residents;
		private PrintWriter out;
		private Path tsvPath;

		private double total = 0.0, totalResidents = 0.0, totalNonResidents = 0.0;

		RoadPricingEventsLogger(Set<Id<Person>> residents) { this.residents = residents; }

		@Override public void notifyStartup(StartupEvent e) {
			try {
				String outDir = e.getServices().getConfig().controller().getOutputDirectory();
				tsvPath = Paths.get(outDir, "personCostEvents.tsv");
				Files.createDirectories(tsvPath.getParent());
				out = new PrintWriter(new FileWriter(tsvPath.toFile()));
				out.println("time\tpersonId\tamount\ttype");   // 与停车方案一致
			} catch (IOException ex) {
				throw new RuntimeException("Cannot open TSV for road pricing events", ex);
			}
		}

		@Override public void handleEvent(PersonMoneyEvent ev) {
			// 兜底：如果某些环境下没触发 startup（不应该），懒初始化一次，避免 NPE
			if (out == null) {
				synchronized (this) {
					if (out == null) {
						try {
							String outDir = ev.getAttributes().getOrDefault("outputDirectory", "").toString();
							if (outDir == null || outDir.isEmpty()) return; // 无法获取则放弃懒初始化
							tsvPath = Paths.get(outDir, "personCostEvents.tsv");
							Files.createDirectories(tsvPath.getParent());
							out = new PrintWriter(new FileWriter(tsvPath.toFile()));
							out.println("time\tpersonId\tamount\ttype");
						} catch (IOException ignore) { return; }
					}
				}
			}

			String purpose = ev.getPurpose()==null ? "" : ev.getPurpose().toLowerCase(Locale.ROOT);
			// 只抓 road pricing 相关事件（不同版本 purpose 可能包含 "road pricing"/"roadpricing"/"toll"）
			if (!(purpose.contains("road") || purpose.contains("toll"))) return;

			double paid = -ev.getAmount(); // 支出→正值
			synchronized (this) { // 简单同步，避免并发写冲突
				out.printf(Locale.ROOT, "%.1f\t%s\t%.2f\t%s%n",
					ev.getTime(), ev.getPersonId(), paid, ev.getPurpose());
			}

			total += paid;
			if (residents.contains(ev.getPersonId())) totalResidents += paid; else totalNonResidents += paid;
		}

		@Override public void reset(int iteration) { /* 单日模型，无需跨日重置 */ }

		@Override public void notifyShutdown(ShutdownEvent e) {
			if (out != null) {
				try { out.flush(); } catch (Exception ignore) {}
				out.close();
			}
			log.info("RoadPricing revenue total: {} € (residents: {} €, non-residents: {} €)",
				String.format(Locale.ROOT,"%.2f", total),
				String.format(Locale.ROOT,"%.2f", totalResidents),
				String.format(Locale.ROOT,"%.2f", totalNonResidents));
		}
	}
}
