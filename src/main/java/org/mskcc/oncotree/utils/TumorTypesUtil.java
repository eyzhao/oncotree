package org.mskcc.oncotree.utils;

import com.univocity.parsers.tsv.TsvParser;
import com.univocity.parsers.tsv.TsvParserSettings;
import org.apache.commons.lang3.StringUtils;
import org.mskcc.oncotree.model.MainType;
import org.mskcc.oncotree.model.TumorType;
import org.mskcc.oncotree.model.TumorTypeQuery;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.core.io.Resource;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by Hongxin on 2/25/16.
 */
public class TumorTypesUtil {
    private static String TUMOR_TREE_FILE = "classpath:tumor_tree.txt";
    private static List<String> TumorTypeKeys = Arrays.asList("code", "name", "nci", "umls", "maintype", "color");

    public static Map<String, TumorType> getTumorTypes() {
        Map<String, TumorType> tumorTypes = new HashMap<>();
        TsvParserSettings settings = new TsvParserSettings();

        //the line separator sequence is defined here to ensure systems such as MacOS and Windows
        //are able to process this file correctly (MacOS uses '\r'; and Windows uses '\r\n').
        settings.getFormat().setLineSeparator("\n");

        // creates a TSV parser
        TsvParser parser = new TsvParser(settings);

        // parses all rows in one go.
        List<String[]> allRows = parser.parseAll(getReader(TUMOR_TREE_FILE));

        TumorType tumorType = new TumorType();
        tumorType.setCode("TISSUE");
        tumorType.setName("Tissue");

        //Iterate each row and assign tumor type to parent following the order of appearing
        for (String[] row : allRows.subList(1, allRows.size())) {
            tumorType.setChildren(attachTumorType(tumorType.getChildren(), row, 0));
        }

        //Attach a root node in the JSON file
        tumorTypes.put("TISSUE", tumorType);
        return tumorTypes;
    }

    public static List<TumorType> findTumorTypes(String key, String keyword, Boolean exactMatch) {
        List<TumorType> tumorTypes = new ArrayList<>();
        key = normalizeTumorTypeKey(key);
        if (TumorTypeKeys.contains(key)) {
            tumorTypes = findTumorType(CacheUtil.getTumorTypes().get("TISSUE"), tumorTypes, key, keyword, exactMatch);
        }
        return tumorTypes;
    }

    public static List<TumorType> findTumorTypes(TumorTypeQuery query) {
        List<TumorType> tumorTypes = new ArrayList<>();
        String key = normalizeTumorTypeKey(query.getType());
        if (TumorTypeKeys.contains(key)) {
            tumorTypes = findTumorType(
                CacheUtil.getTumorTypes().get("TISSUE"), tumorTypes, key, query.getQuery(), query.getExactMatch());
        }
        return tumorTypes;
    }

    public static InputStream getTumorTypeInputStream() {
        return getInputStream(TUMOR_TREE_FILE);
    }

    private static List<TumorType> findTumorType(TumorType allTumorTypes, List<TumorType> matchedTumorTypes,
                                                 String key, String keyword, Boolean exactMatch) {
        Map<String, TumorType> childrenTumorTypes = allTumorTypes.getChildren();
        Boolean match = false;

        if (exactMatch == null) {
            exactMatch = true;
        }

        switch (key) {
            case "code":
                if (exactMatch) {
                    match = allTumorTypes.getCode() == null ? false : allTumorTypes.getCode().equalsIgnoreCase(keyword);
                } else {
                    match = allTumorTypes.getCode() == null ?
                        false :
                        StringUtils.containsIgnoreCase(allTumorTypes.getCode(), keyword);
                }
                break;
            case "color":
                if (exactMatch) {
                    match = allTumorTypes.getColor() == null ? false : allTumorTypes.getColor().equalsIgnoreCase(keyword);
                } else {
                    match = allTumorTypes.getColor() == null ?
                        false :
                        StringUtils.containsIgnoreCase(allTumorTypes.getColor(), keyword);
                }
                break;
            case "name":
                if (exactMatch) {
                    match = allTumorTypes.getName() == null ? false : allTumorTypes.getName().equalsIgnoreCase(keyword);
                } else {
                    match = allTumorTypes.getName() == null ?
                        false :
                        StringUtils.containsIgnoreCase(allTumorTypes.getName(), keyword);
                }
                break;
            case "nci":
                if (exactMatch) {
                    match = allTumorTypes.getNCI() == null ? false : allTumorTypes.getNCI().equalsIgnoreCase(keyword);
                } else {
                    match = allTumorTypes.getNCI() == null ?
                        false :
                        StringUtils.containsIgnoreCase(allTumorTypes.getNCI(), keyword);
                }
                break;
            case "umls":
                if (exactMatch) {
                    match = allTumorTypes.getUMLS() == null ? false : allTumorTypes.getUMLS().equalsIgnoreCase(keyword);
                } else {
                    match = allTumorTypes.getUMLS() == null ?
                        false :
                        StringUtils.containsIgnoreCase(allTumorTypes.getUMLS(), keyword);
                }
                break;
            default:
                if (exactMatch) {
                    match = allTumorTypes.getCode() == null ? false : allTumorTypes.getCode().equalsIgnoreCase(keyword);
                } else {
                    match = allTumorTypes.getCode() == null ?
                        false :
                        StringUtils.containsIgnoreCase(allTumorTypes.getCode(), keyword);
                }
        }

        if (match) {
            TumorType tumorType = new TumorType();
            tumorType.setCode(allTumorTypes.getCode());
            tumorType.setName(allTumorTypes.getName());
            tumorType.setUMLS(allTumorTypes.getUMLS());
            tumorType.setNCI(allTumorTypes.getNCI());
            tumorType.setMainType(allTumorTypes.getMainType());
            tumorType.setColor(allTumorTypes.getColor());

            matchedTumorTypes.add(tumorType);
        }

        if (childrenTumorTypes.size() > 0) {
            Iterator it = childrenTumorTypes.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry pair = (Map.Entry) it.next();
                matchedTumorTypes = findTumorType((TumorType) pair.getValue(), matchedTumorTypes, key, keyword, exactMatch);
            }
        }
        return matchedTumorTypes;
    }

    private static String normalizeTumorTypeKey(String key) {
        key = key.toLowerCase();
        key.replaceAll("[^a-z]+", "");
        return key;
    }

    /**
     * Attach children to parent node.
     *
     * @param tumorTypes
     * @param row        One row of text file
     * @param index      Current index of row. It will be increased everytime this function has been called.
     * @return parent node.
     */
    private static Map<String, TumorType> attachTumorType(Map<String, TumorType> tumorTypes, String[] row, int index) {
        if (index < 5 && row.length > index && row[index] != null && !row[index].isEmpty()) {
            Map<String, String> result = parseCodeName(row[index]);
            if (result.containsKey("code")) {
                String code = result.get("code");
                TumorType tumorType = new TumorType();
                if (!tumorTypes.containsKey(code)) {
                    MainType mainType = new MainType();
                    mainType.setName(row.length > 5 ? row[5] : "");

                    tumorType.setCode(code);
                    tumorType.setName(result.get("name"));
                    tumorType.setMainType(mainType);
                    tumorType.setColor(row.length > 6 ? row[6] : "");
                    tumorType.setNCI(row.length > 7 ? row[7] : "");
                    tumorType.setUMLS(row.length > 8 ? row[8] : "");
                } else {
                    tumorType = tumorTypes.get(code);
                }
                tumorType.setChildren(attachTumorType(tumorType.getChildren(), row, ++index));
                tumorTypes.put(code, tumorType);
            }
        }
        return tumorTypes;
    }

    /**
     * Parsing cell content into tumor type code and tumor type name.
     *
     * @param content One cell of each row.
     * @return The map of current tumor type. It includes 'code' and 'name'.
     */
    private static HashMap<String, String> parseCodeName(String content) {
        HashMap<String, String> result = new HashMap<>();

        Pattern pattern = Pattern.compile("([^\\(]+)\\(([^\\)]+)\\)");

        Matcher matcher = pattern.matcher(content);
        if (matcher.matches()) {
            result.put("name", matcher.group(1).trim());
            result.put("code", matcher.group(2).trim());
        }
        return result;
    }

    /**
     * Get tumor type text file input stream.
     *
     * @param relativePath Tumor type text file path.
     * @return Input stream
     */
    private static InputStreamReader getReader(String relativePath) {
        try {
            return new InputStreamReader(getInputStream(relativePath), "UTF-8");
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        return null;
    }

    private static InputStream getInputStream(String relativePath) {
        ApplicationContext applicationContext = new ClassPathXmlApplicationContext();
        Resource resource = applicationContext.getResource(relativePath);
        try {
            return resource.getInputStream();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}