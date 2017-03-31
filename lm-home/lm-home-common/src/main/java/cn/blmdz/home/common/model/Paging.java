package cn.blmdz.home.common.model;

import com.google.common.base.MoreObjects;
import com.google.common.collect.Lists;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * 数据分页对象
 * email:dong_peiji@huateng.com
 * Created by 董培基 on 2016/3/2.
 */
public class Paging<T> implements Serializable {

    private static final long serialVersionUID = -1809752222912021528L;

    private Long total;

    private List<T> datas;


    public Long getTotal() {
        return total;
    }

    public void setTotal(Long total) {
        this.total = total;
    }

    public List<T> getDatas() {
        return datas;
    }

    public void setDatas(List<T> datas) {
        this.datas = datas;
    }

    public Paging() {
    }

    public Paging(Long total, List<T> datas) {
        this.datas = datas;
        this.total = total;
    }

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
