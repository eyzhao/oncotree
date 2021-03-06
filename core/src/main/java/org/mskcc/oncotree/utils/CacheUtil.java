package org.mskcc.oncotree.utils;

import org.mskcc.oncotree.error.InvalidOncoTreeDataException;
import org.mskcc.oncotree.model.MainType;
import org.mskcc.oncotree.model.TumorType;
import org.mskcc.oncotree.model.Version;

import java.util.*;

/**
 * Created by Hongxin on 2/25/16.
 */
public class CacheUtil {
    public static Map<Version, List<MainType>> mainTypes = new HashMap<>();
    public static Map<Version, Map<String, TumorType>> tumorTypes = new HashMap<>();

    public static void resetTumorTypesByVersion(Version version) {
        if (tumorTypes.containsKey(version)) {
            tumorTypes.get(version).clear();
        } else {
            tumorTypes.put(version, new HashMap<String, TumorType>());
        }
    }

    public static Map<String, TumorType> getOrResetTumorTypesByVersion(Version version) throws InvalidOncoTreeDataException {
        if (version != null) {
            if(version.getVersion() == "realtime") {
                tumorTypes.remove(version);
            }
            return getTumorTypesByVersion(version);
        } else {
            return new HashMap<>();
        }
    }

    public static Map<String, TumorType> getTumorTypesByVersion(Version version) throws InvalidOncoTreeDataException {
        if (tumorTypes.containsKey(version)) {
            return tumorTypes.get(version);
        } else {
            tumorTypes.put(version, TumorTypesUtil.getTumorTypesByVersionFromRaw(version));
            return tumorTypes.get(version);
        }
    }

    public static List<MainType> getMainTypesByVersion(Version version) throws InvalidOncoTreeDataException {
        if (!mainTypes.containsKey(version)) {
            mainTypes.put(version, new ArrayList<MainType>());
        }
        return mainTypes.get(version);
    }

    public static void addMainTypeByVersion(Version version, MainType mainType) {
        if (!mainTypes.containsKey(version)) {
            mainTypes.put(version, new ArrayList<MainType>());
        }
        mainTypes.get(version).add(mainType);
    }

    public static void resetMainTypesByVersion(Version version) {
        if (mainTypes.containsKey(version)) {
            mainTypes.get(version).clear();
        } else {
            mainTypes.put(version, new ArrayList<MainType>());
        }
    }

    public static MainType getMainTypeByVersion(Version version, Integer id) {
        List<MainType> selectedMainTypes = mainTypes.get(version);
        if (selectedMainTypes != null) {
            for (MainType mainType : selectedMainTypes) {
                if (mainType.getId().equals(id)) {
                    return mainType;
                }
            }
        }
        return null;
    }
}
