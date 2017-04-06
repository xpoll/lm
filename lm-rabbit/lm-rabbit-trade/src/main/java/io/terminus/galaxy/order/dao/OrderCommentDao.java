package io.terminus.galaxy.order.dao;

import com.google.common.base.Objects;
import com.google.common.collect.ImmutableMap;
import io.terminus.common.model.Paging;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.galaxy.order.model.OrderComment;
import org.springframework.stereotype.Repository;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Author:cp
 * Created on 4/22/16
 */
@Repository
public class OrderCommentDao extends MyBatisDao<OrderComment> {

    /**
     * 评价列表
     *
     * @param params 查询参数
     * @return 分页后的评价列表
     */
    public Paging<OrderComment> pagingFindBy(Map<String, Object> params) {
        Long count = getSqlSession().selectOne(sqlId("countBy"), params);
        if (Objects.equal(count, 0L)) {
            return new Paging<>(0L, Collections.<OrderComment>emptyList());
        }
        List<OrderComment> data = getSqlSession().selectList(sqlId("findBy"), params);
        return new Paging<>(count, data);
    }

    /**
     * 获取评价数量
     *
     * @param params 查询参数
     * @return 评价数量
     */
    public Long count(Map<String, Object> params) {
        return getSqlSession().selectOne(sqlId("countBy"), params);
    }

    /**
     * 获取评价最大id
     */
    public Long maxId() {
        return getSqlSession().selectOne(sqlId("maxId"));
    }

    /**
     * 批量修改评价状态
     *
     * @param ids    评价id列表
     * @param status 评价状态
     */
    public void batchUpdateStatus(List<Long> ids, Integer status) {
        getSqlSession().update(sqlId("batchUpdateStatus"), ImmutableMap.of(
                "ids", ids, "status", status
        ));
    }

    /**
     * 根据子订单id查询评价
     *
     * @param orderItemIds 子订单id
     * @return 评价列表
     */
    public List<OrderComment> findByOrderItemIds(Long... orderItemIds) {
        return getSqlSession().selectList(sqlId("findByOrderItemIds"), orderItemIds);
    }

    /**
     * 查询卖家所有已评价的商品id
     *
     * @param sellerId 卖家id
     */
    public Paging<Long> pagingDistinctItemIdBySellerId(Long sellerId, Integer offset, Integer limit) {
        Long count = getSqlSession().selectOne(sqlId("countDistinctItemIdBySellerId"), sellerId);
        if (Objects.equal(count, 0L)) {
            return new Paging<>(0L, Collections.<Long>emptyList());
        }
        List<Long> data = getSqlSession().selectList(sqlId("findDistinctItemIdBySellerId"), ImmutableMap.of(
                "offset", offset, "limit", limit, "sellerId", sellerId
        ));
        return new Paging<>(count, data);
    }

    public Long countBy(Map<String, Object> params) {
        return getSqlSession().selectOne(sqlId("countBy"), params);
    }
}