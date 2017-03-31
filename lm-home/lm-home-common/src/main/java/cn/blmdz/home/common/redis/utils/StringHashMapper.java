package cn.blmdz.home.common.redis.utils;

import com.fasterxml.jackson.databind.JavaType;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.util.JsonMapper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * email:dong_peiji@huateng.com
 * Created by 董培基 on 2016/3/3.
 */
public class StringHashMapper<T> {
    private final JsonMapper mapper = JsonMapper.nonDefaultMapper();
    private final JavaType userType;
    private final JavaType mapType;

    public StringHashMapper(Class<T> type) {
        this.userType = this.mapper.createCollectionType(HashMap.class, String.class, String.class);
        this.mapType = this.mapper.getMapper().constructType(type);
    }

    public T fromHash(Map<String, String> hash) {
        return hash.isEmpty() ? null : (T) this.mapper.getMapper().convertValue(hash, this.userType);
    }

    public Map<String, String> toHash(T object) {
        Map hash = (Map) this.mapper.getMapper().convertValue(object, this.mapType);
        ArrayList nullKeys = Lists.newArrayListWithCapacity(hash.size());
        Iterator var4 = hash.entrySet().iterator();

        while (var4.hasNext()) {
            Map.Entry nullKey = (Map.Entry) var4.next();
            if (nullKey.getValue() == null) {
                nullKeys.add(nullKey.getKey());
            }
        }

        var4 = nullKeys.iterator();

        while (var4.hasNext()) {
            String nullKey1 = (String) var4.next();
            hash.remove(nullKey1);
        }

        return hash;
    }

}
