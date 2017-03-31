package cn.blmdz.home.common.util;

import com.google.common.base.Predicate;
import com.google.common.base.Strings;
import com.google.common.collect.Maps;

import java.util.Map;

/**
 * email:dong_peiji@huateng.com
 * Created by 董培基 on 2016/3/3.
 */
public class Params {

    public static Map<String, Object> filterNullOrEmpty(Map<String, Object> criteria) {
        return Maps.filterEntries(criteria, new Predicate<Map.Entry<String, Object>>() {
            @Override
            public boolean apply(Map.Entry<String, Object> stringObjectEntry) {
                Object value = stringObjectEntry.getValue();
                return value instanceof String ? !Strings.isNullOrEmpty((String) value) : value != null;
            }
        });
    }

    public static String trimToNull(String str) {
        return str != null?Strings.emptyToNull(str.replace(' ', ' ').trim()):null;
    }

    public static String trimToNull(Object obj) {
        return obj != null?trimToNull(obj.toString()):null;
    }

}
