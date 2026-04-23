package playground.vsp.simpleParkingCostHandler;

import org.matsim.core.config.ReflectiveConfigGroup;

import java.util.HashSet;
import java.util.Set;

public class ParkingCostConfigGroup extends ReflectiveConfigGroup {

	public static final String GROUP_NAME = "parkingCost";

	// 参数 keys
	public static final String MODE = "mode";
	public static final String RATE_ATTRIBUTE = "rateAttribute";
	public static final String EXCLUDED_PREFIXES = "excludedPrefixes";

	private String mode = "car";
	private String rateAttribute = "parking_rate";
	private final Set<String> excludedPrefixes = new HashSet<>();

	public ParkingCostConfigGroup() {
		super(GROUP_NAME);
	}

	@StringGetter(MODE)
	public String getMode() {
		return mode;
	}

	@StringSetter(MODE)
	public void setMode(String mode) {
		this.mode = mode;
	}

	@StringGetter(RATE_ATTRIBUTE)
	public String getRateAttributeName() {
		return rateAttribute;
	}

	@StringSetter(RATE_ATTRIBUTE)
	public void setRateAttributeName(String rateAttribute) {
		this.rateAttribute = rateAttribute;
	}

	@StringGetter(EXCLUDED_PREFIXES)
	public String getExcludedPrefixesAsString() {
		return String.join(",", excludedPrefixes);
	}

	@StringSetter(EXCLUDED_PREFIXES)
	public void setExcludedPrefixesFromString(String s) {
		this.excludedPrefixes.clear();
		if (s != null && !s.isBlank()) {
			for (String prefix : s.split(",")) {
				this.excludedPrefixes.add(prefix.trim());
			}
		}
	}

	public Set<String> getExcludedPrefixes() {
		return excludedPrefixes;
	}
}


