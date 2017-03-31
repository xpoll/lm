package cn.blmdz.home.common.util;

import com.google.common.base.Joiner;

/**
 * email:dong_peiji@huateng.com
 * Created by 董培基 on 2016/3/9.
 */
public class Joiners {
    public static final Joiner DOT = Joiner.on(".").skipNulls();
    public static final Joiner COMMA = Joiner.on(",").skipNulls();
    public static final Joiner COLON = Joiner.on(":").skipNulls();
}
