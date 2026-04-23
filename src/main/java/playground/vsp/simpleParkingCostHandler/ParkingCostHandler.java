package playground.vsp.simpleParkingCostHandler;

import com.google.inject.Inject;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.events.*;
import org.matsim.api.core.v01.events.handler.*;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.population.Person;
import org.matsim.core.api.experimental.events.EventsManager;
import org.matsim.core.controler.events.ShutdownEvent;
import org.matsim.core.controler.listener.ShutdownListener;

import java.util.*;

public final class ParkingCostHandler implements
	PersonLeavesVehicleEventHandler,
	PersonEntersVehicleEventHandler,
	PersonDepartureEventHandler,
	TransitDriverStartsEventHandler,
	ActivityEndEventHandler,
	ShutdownListener {

	private final Scenario scenario;
	private final EventsManager events;
	private final ParkingCostConfigGroup config;

	private final Map<Id<Person>, Double> lastLeaveTime = new HashMap<>();
	private final Map<Id<Person>, Id<Link>> lastParkedLink = new HashMap<>();
	private final Map<Id<Person>, String> lastActivityType = new HashMap<>();
	private final Set<Id<Person>> ptDrivers = new HashSet<>();
	private double totalCost = 0.0;

	@Inject
	public ParkingCostHandler(Scenario scenario, EventsManager events, ParkingCostConfigGroup config) {
		this.scenario = scenario;
		this.events = events;
		this.config = config;
	}

	@Override
	public void reset(int iteration) {
		lastLeaveTime.clear();
		lastParkedLink.clear();
		lastActivityType.clear();
		ptDrivers.clear();
		totalCost = 0.0;
	}

	@Override
	public void handleEvent(TransitDriverStartsEvent event) {
		ptDrivers.add(event.getDriverId());
	}

	@Override
	public void handleEvent(ActivityEndEvent event) {
		if (ptDrivers.contains(event.getPersonId())) return;
		lastActivityType.put(event.getPersonId(), event.getActType());
	}

	@Override
	public void handleEvent(PersonDepartureEvent event) {
		if (ptDrivers.contains(event.getPersonId())) return;
		if (!event.getLegMode().equals(config.getMode())) return;

		lastParkedLink.put(event.getPersonId(), event.getLinkId());
	}

	@Override
	public void handleEvent(PersonLeavesVehicleEvent event) {
		if (ptDrivers.contains(event.getPersonId())) return;
		lastLeaveTime.put(event.getPersonId(), event.getTime());
	}

	@Override
	public void handleEvent(PersonEntersVehicleEvent event) {
		if (ptDrivers.contains(event.getPersonId())) return;

		Id<Person> personId = event.getPersonId();
		double endTime = event.getTime();
		double startTime = lastLeaveTime.getOrDefault(personId, endTime);
		double durationH = (endTime - startTime) / 3600.0;
		if (durationH <= 0) return;

		String actType = lastActivityType.getOrDefault(personId, "");
		for (String prefix : config.getExcludedPrefixes()) {
			if (actType.startsWith(prefix)) return;
		}

		Id<Link> linkId = lastParkedLink.get(personId);
		if (linkId == null) return;

		Link link = scenario.getNetwork().getLinks().get(linkId);
		if (link == null) return;

		Object attr = link.getAttributes().getAttribute(config.getRateAttributeName());
		if (!(attr instanceof Double rate) || rate <= 0) return;

		double billedHours = Math.ceil(durationH);
		double cost = -rate * billedHours;

		// Purpose description for later differentiation
		String purposeDescription = (startTime < 3 * 3600.0 || endTime > 21 * 3600.0) ? "overnight" : "normal";

		events.processEvent(new PersonMoneyEvent(
			endTime,
			personId,
			cost,
			"parking",
			linkId.toString(),
			purposeDescription
		));

		totalCost += -cost; // accumulate total positive cost
	}

	@Override
	public void notifyShutdown(ShutdownEvent event) {
		System.out.printf("[ParkingCostHandler] Total parking charge applied: %.2f €\n", totalCost);
	}
}
