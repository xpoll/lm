package cn.blmdz.home.common.model;

import com.google.common.base.MoreObjects;
import com.google.common.base.Strings;
import com.google.common.collect.Maps;

import java.util.HashMap;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

public class PageInfo {
    public static final String LIMIT = "limit";
    public static final String OFFSET = "offset";
    @Getter
    @Setter
    private Integer offset;
    @Getter
    @Setter
    private Integer limit;

    public PageInfo() {
    }

    public static PageInfo of(Integer pageNo, Integer size) {
        return new PageInfo(pageNo, size);
    }

    public PageInfo(Integer pageNo, Integer size) {
        pageNo = MoreObjects.firstNonNull(pageNo, 1);
        size = MoreObjects.firstNonNull(size, 20);
        this.limit = size > 0 ? size : 20;
        this.offset = (pageNo - 1) * size;
        this.offset = offset > 0 ? offset : 0;
    }

    /**
     * 转换不同数据库 分页 限制长度以及开始行 关键字
     *
     * @param limit  限制条数描述 mysql：limit oracle
     * @param offset
     * @return
     */
    public Map<String, Object> toMap(String limit, String offset) {
        HashMap<String, Object> paraMap = Maps.newHashMapWithExpectedSize(2);
        paraMap.put(Strings.isNullOrEmpty(limit) ? LIMIT : limit, this.limit);
        paraMap.put(Strings.isNullOrEmpty(offset) ? OFFSET : offset, this.offset);
        return paraMap;
    }

    public Map<String, Object> toMap() {
        return toMap(null, null);
    }
}
