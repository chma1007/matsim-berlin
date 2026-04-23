package org.matsim.prepare;

import org.locationtech.jts.geom.prep.PreparedGeometry;
import org.matsim.api.core.v01.Coord;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.contrib.roadpricing.RoadPricingScheme;
import org.matsim.contrib.roadpricing.RoadPricingSchemeImpl;
import org.matsim.contrib.roadpricing.RoadPricingUtils;
import org.matsim.contrib.roadpricing.RoadPricingWriterXMLv1;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.network.NetworkUtils;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.core.utils.geometry.CoordinateTransformation;
import org.matsim.core.utils.geometry.transformations.TransformationFactory;
import org.matsim.core.utils.io.IOUtils;
import org.matsim.core.utils.misc.Time;
import org.matsim.utils.gis.shp2matsim.ShpGeometryUtils;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class CreateRoadPricingXml {

	private static final String NETWORK_CRS = "EPSG:25832";
	private static final String SHP_CRS = "EPSG:31468";

	public static void main(String[] args) throws Exception {
		// 输入路径
		String networkFile = "D:/berlin/input/v6.4/berlin-v6.4-network-with-pt.xml.gz";
		String zoneShpFile = "https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/berlin/projects/avoev/shp-files/shp-inner-city-area/inner-city-area.shp";

		// 读取网络
		Network network = NetworkUtils.readNetwork(networkFile);

		// 加载 shapefile 多边形
		List<PreparedGeometry> geometries = ShpGeometryUtils.loadPreparedGeometries(IOUtils.resolveFileOrResource(zoneShpFile));
		CoordinateTransformation transformer = TransformationFactory.getCoordinateTransformation(NETWORK_CRS, SHP_CRS);

		// 找出边界链接（from 与 to 只有一个在区域内）
		Set<Id<Link>> tolledLinks = new HashSet<>();
		for (Link link : network.getLinks().values()) {
			if (!link.getAllowedModes().contains("pt")) {
				Coord fromNode = transformer.transform(link.getFromNode().getCoord());
				Coord toNode = transformer.transform(link.getToNode().getCoord());
				boolean inFrom = ShpGeometryUtils.isCoordInPreparedGeometries(fromNode, geometries);
				boolean inTo = ShpGeometryUtils.isCoordInPreparedGeometries(toNode, geometries);
				if (inFrom ^ inTo) {
					tolledLinks.add(link.getId());
				}
			}
		}

		// 使用 contrib 风格：€/km，从 0 到 25 每 2.5 增长
		for (double ii = 0.0; ii <= 25.0; ii += 2.5) {
			double toll = ii / 1000.0;  // €/m
			String outFile = String.format("hundekopf_distance_roadpricing_%.1f_euro_per_km.xml", ii);
			writeDistancePricing(tolledLinks, toll, outFile);
		}

		System.out.println("✅ Distance-based roadpricing XMLs written.");
	}

	private static void writeDistancePricing(Set<Id<Link>> tolledLinks, double euroPerMeter, String outFile) {
		Scenario scenario = ScenarioUtils.createScenario(ConfigUtils.createConfig());
		RoadPricingSchemeImpl scheme = RoadPricingUtils.addOrGetMutableRoadPricingScheme(scenario);

		RoadPricingUtils.setType(scheme, RoadPricingScheme.TOLL_TYPE_DISTANCE);
		RoadPricingUtils.setName(scheme, "Hundekopf_distance_toll");
		RoadPricingUtils.setDescription(scheme, "distance-based toll at Berlin inner city border");

		tolledLinks.forEach(linkId -> RoadPricingUtils.addLink(scheme, linkId));

		RoadPricingUtils.createAndAddGeneralCost(scheme,
			Time.parseTime("00:00:00"),
			Time.parseTime("30:00:00"),
			euroPerMeter);

		new RoadPricingWriterXMLv1(scheme).writeFile(outFile);
	}
}
