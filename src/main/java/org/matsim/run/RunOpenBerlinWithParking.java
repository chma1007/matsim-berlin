package org.matsim.run;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.geotools.api.feature.simple.SimpleFeature;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.prep.PreparedGeometry;
import org.locationtech.jts.geom.prep.PreparedGeometryFactory;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.population.*;
import org.matsim.api.core.v01.events.PersonMoneyEvent;
import org.matsim.api.core.v01.events.handler.PersonMoneyEventHandler;
import org.matsim.core.config.Config;
import org.matsim.core.controler.AbstractModule;
import org.matsim.core.controler.Controler;
import org.matsim.core.controler.events.ShutdownEvent;
import org.matsim.core.controler.events.StartupEvent;
import org.matsim.core.controler.listener.ShutdownListener;
import org.matsim.core.controler.listener.StartupListener;
import org.matsim.core.scenario.MutableScenario;
import org.matsim.core.utils.geometry.geotools.MGC;
import org.matsim.core.utils.gis.ShapeFileReader;
import playground.vsp.simpleParkingCostHandler.ParkingCostConfigGroup;
import playground.vsp.simpleParkingCostHandler.ParkingCostModule;
import org.matsim.analysis.personMoney.PersonMoneyEventsAnalysisModule;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

public class RunOpenBerlinWithParking extends OpenBerlinScenario {

	private static final Logger log =
		LogManager.getLogger(RunOpenBerlinWithParking.class);

	private static final String HUNDEKOPF_SHP =
		"D:/berlin/input/v6.4/berlin hundekopf/berlin_hundekopf_ONLY_25832.shp";

	private static final double RATE = 2.5;
	private static final String RATE_ATTR = "parking_rate";

	public static void main(String[] args) {
		run(RunOpenBerlinWithParking.class, args);
	}

	@Override
	protected Config prepareConfig(Config config) {
		ParkingCostConfigGroup pc = new ParkingCostConfigGroup();
		pc.setMode("car");
		pc.setRateAttributeName(RATE_ATTR);
		pc.setExcludedPrefixesFromString(""); // home 也收费
		config.addModule(pc);
		return super.prepareConfig(config);
	}

	@Override
	protected void prepareControler(Controler controler) {
		super.prepareControler(controler);

		MutableScenario scenario = (MutableScenario) controler.getScenario();
		PreparedGeometry zone = loadZone(HUNDEKOPF_SHP);

		// ========= ① 只标记 Hundekopf 内的 car link =========
		int tagged = 0;

		for (Link link : scenario.getNetwork().getLinks().values()) {

			// ⭐ 关键：只对 car link 处理
			if (!link.getAllowedModes().contains("car")) {
				continue;
			}

			Point pt = MGC.coord2Point(link.getCoord());
			if (zone.covers(pt)) {
				link.getAttributes().putAttribute(RATE_ATTR, RATE);
				tagged++;
			}
		}

		log.info(
			"Tagged {} CAR links within Hundekopf with parking_rate={} €/h.",
			tagged, RATE
		);

		// ========= ② 标记 resident =========
		controler.addControlerListener((StartupListener) event -> {
			int total = 0;
			int residents = 0;

			for (Person p : scenario.getPopulation().getPersons().values()) {
				total++;
				boolean isResident = false;

				for (PlanElement pe : p.getSelectedPlan().getPlanElements()) {
					if (pe instanceof Activity act
						&& act.getType().startsWith("home")) {

						Link homeLink =
							scenario.getNetwork().getLinks().get(act.getLinkId());

						if (homeLink != null) {
							Point pt = MGC.coord2Point(homeLink.getCoord());
							if (zone.covers(pt)) {
								isResident = true;
								break;
							}
						}
					}
				}

				p.getAttributes().putAttribute("resident", isResident);
				if (isResident) {
					residents++;
				}
			}

			log.info("Marked residents: {} / {}", residents, total);
		});

		// ========= ③ 注册 ParkingCostModule =========
		controler.addOverridingModule(new ParkingCostModule());

		// ========= ④ 注册 PersonMoneyEventsAnalysisModule =========
		controler.addOverridingModule(new PersonMoneyEventsAnalysisModule());

		// ========= ⑤ 自定义 Tracker =========
		ParkingTracker tracker = new ParkingTracker(
			scenario,
			controler.getConfig().controller().getOutputDirectory()
		);

		controler.addOverridingModule(new AbstractModule() {
			@Override
			public void install() {
				addEventHandlerBinding().toInstance(tracker);
				addControlerListenerBinding().toInstance(tracker);
			}
		});
	}

	private PreparedGeometry loadZone(String shp) {
		Collection<SimpleFeature> fs = ShapeFileReader.getAllFeatures(shp);
		if (fs.isEmpty()) {
			throw new RuntimeException("Empty shapefile: " + shp);
		}
		Geometry g = (Geometry) fs.iterator().next().getDefaultGeometry();
		return PreparedGeometryFactory.prepare(g);
	}

	// ========= ParkingTracker =========
	static class ParkingTracker implements
		PersonMoneyEventHandler,
		StartupListener,
		ShutdownListener {

		private final MutableScenario scenario;
		private final String outDir;

		private PrintWriter writer;
		private PrintWriter writerByActivity;
		private double total = 0.0;

		public ParkingTracker(MutableScenario scenario, String outDir) {
			this.scenario = scenario;
			this.outDir = outDir;
		}

		@Override
		public void notifyStartup(StartupEvent event) {
			try {
				writer = new PrintWriter(
					new FileWriter(outDir + "/parkingEvents.tsv")
				);
				writer.println(
					"time\tperson\tresident\tcost\tduration_h\tlinkId\tpurpose"
				);

				writerByActivity = new PrintWriter(
					new FileWriter(outDir + "/parkingEvents_byActivity.tsv")
				);
				writerByActivity.println(
					"time\tperson\tactivityType\tcost\tlinkId"
				);

			} catch (IOException e) {
				throw new RuntimeException("Cannot open output TSV", e);
			}
		}

		@Override
		public void handleEvent(PersonMoneyEvent e) {
			if (!e.getPurpose().contains("parking")) return;

			double cost = -e.getAmount();
			if (cost <= 0) return;

			Person p =
				scenario.getPopulation().getPersons().get(e.getPersonId());
			boolean resident =
				(boolean) p.getAttributes().getAttribute("resident");

			double duration_h = cost / RATE;

			writer.printf(
				Locale.ROOT,
				"%.1f\t%s\t%b\t%.2f\t%.2f\t%s\t%s%n",
				e.getTime(),
				e.getPersonId(),
				resident,
				cost,
				duration_h,
				e.getTransactionPartner(),
				e.getPurpose()
			);

			String actType = findLastActivityType(p, e.getTime());
			writerByActivity.printf(
				Locale.ROOT,
				"%.1f\t%s\t%s\t%.2f\t%s%n",
				e.getTime(),
				e.getPersonId(),
				actType,
				cost,
				e.getTransactionPartner()
			);

			total += cost;
		}

		private String findLastActivityType(Person person, double time) {
			Plan plan = person.getSelectedPlan();
			Activity lastAct = null;

			for (PlanElement pe : plan.getPlanElements()) {
				if (pe instanceof Activity act) {
					if (act.getEndTime().isDefined()
						&& act.getEndTime().seconds() <= time) {
						lastAct = act;
					}
				}
			}
			return lastAct != null ? lastAct.getType() : "unknown";
		}

		@Override public void reset(int iteration) {}

		@Override
		public void notifyShutdown(ShutdownEvent event) {
			if (writer != null) writer.close();
			if (writerByActivity != null) writerByActivity.close();
			log.info(
				"Total parking charged = {} €",
				String.format(Locale.ROOT, "%.2f", total)
			);
		}
	}
}
