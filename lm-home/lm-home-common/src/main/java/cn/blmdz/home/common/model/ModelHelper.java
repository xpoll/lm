package cn.blmdz.home.common.model;

import com.google.common.base.Strings;
import com.google.common.collect.Lists;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 模型模板类
 * email:dong_peiji@huateng.com
 * Created by 董培基 on 2016/3/2.
 */
public class ModelHelper {

    public ModelHelper() {
    }

    /**
     * 获取主键Id
     *
     * @param indexables 需要索引的集合
     * @return 主键id集合
     */
    public static List<Long> extraIds(List<? extends Indexable> indexables) {
        List<Long> longList = Lists.newArrayList();
        for (Indexable indexable : indexables) {
            longList.add(indexable.getId());
        }
        return longList;
    }

    /**
     * 获取索引集合对象中fieldName字段的值
     *
     * @param models    索引集合
     * @param fieldName 查询的字段
     * @param <T>       返回的结果类型
     * @return 返回的结果集合
     */
    public static <T> List<T> extractField(List<? extends Serializable> models, String fieldName) {
        if (null != models && !models.isEmpty() && !Strings.isNullOrEmpty(fieldName)) {
            Class sourceClass = ((Serializable) models.get(0)).getClass();
            ArrayList fields = Lists.newArrayList();

            try {
                Field field = sourceClass.getDeclaredField(fieldName);
                boolean accessible = field.isAccessible();
                field.setAccessible(true);
                for (Object model : models) {
                    Object value = field.get(model);
                    fields.add(value);
                }
                field.setAccessible(accessible);
            } catch (NoSuchFieldException | IllegalAccessException e) {
                e.fillInStackTrace();
            }
            return fields;
        } else {
            return Collections.emptyList();
        }
    }
}