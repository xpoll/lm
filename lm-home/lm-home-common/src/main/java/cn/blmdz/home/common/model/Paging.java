package cn.blmdz.home.common.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * 数据分页对象
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class Paging<T> implements Serializable {

    private static final long serialVersionUID = -1809752222912021528L;
    
    private Long total;
    private List<T> datas;


    public Boolean isEmpty() {
        return Objects.equals(0L, total) || null == datas || datas.isEmpty();
    }

    public static <T> Paging<T> empty() {
        return new Paging<>(0L, Collections.<T>emptyList());
    }

    public static <T> Paging<T> empty(Class<T> tClass) {
        return new Paging<>(0L, Collections.<T>emptyList());
    }

}
