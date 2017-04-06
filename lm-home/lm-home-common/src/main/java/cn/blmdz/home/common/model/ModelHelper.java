package cn.blmdz.home.common.model;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;

import com.google.common.base.Strings;
import com.google.common.collect.Lists;

/**
 * 模型模板类
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
    public static List<Long> extradIds(List<? extends Indexable> indexables) {
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
    public static <T> List<T> extractFiled(List<? extends Serializable> models, String fieldName) {
        if (null != models && !models.isEmpty() && !Strings.isNullOrEmpty(fieldName)) {
            Class<? extends Serializable> sourceClass = ((Serializable) models.get(0)).getClass();
            List<T> fields = Lists.newArrayList();

            try {
                Field field = sourceClass.getDeclaredField(fieldName);
                boolean accessible = field.isAccessible();
                field.setAccessible(true);
                for (Object model : models) {
                    @SuppressWarnings("unchecked")
					T value = (T) field.get(model);
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