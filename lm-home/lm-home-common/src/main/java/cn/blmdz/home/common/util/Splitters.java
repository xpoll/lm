package cn.blmdz.home.common.util;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * email:dong_peiji@huateng.com
 * Created by 董培基 on 2016/3/3.
 */
public class Splitters {
    public static final Splitter DOT = Splitter.on(".").omitEmptyStrings().trimResults();
    public static final Splitter COMMA = Splitter.on(",").omitEmptyStrings().trimResults();
    public static final Splitter COLON = Splitter.on(":").omitEmptyStrings().trimResults();
    public static final Splitter AT = Splitter.on("@").omitEmptyStrings().trimResults();
    public static final Splitter SLASH = Splitter.on("/").omitEmptyStrings().trimResults();
    public static final Splitter SPACE = Splitter.on(" ").omitEmptyStrings().trimResults();
    public static final Splitter UNDERSCORE = Splitter.on("_").omitEmptyStrings().trimResults();

    public Splitters() {
    }

    public static List<Long> splitToLong(CharSequence sequence, Splitter splitter) {
        List ss = splitter.splitToList(sequence);
        ArrayList res = Lists.newArrayListWithCapacity(ss.size());
        Iterator var4 = ss.iterator();

        while(var4.hasNext()) {
            String s = (String)var4.next();
            res.add(Long.valueOf(Long.parseLong(s)));
        }

        return res;
    }

    public static List<Integer> splitToInteger(CharSequence sequence, Splitter splitter) {
        List ss = splitter.splitToList(sequence);
        ArrayList res = Lists.newArrayListWithCapacity(ss.size());
        Iterator var4 = ss.iterator();

        while(var4.hasNext()) {
            String s = (String)var4.next();
            res.add(Integer.valueOf(Integer.parseInt(s)));
        }

        return res;
    }
}
