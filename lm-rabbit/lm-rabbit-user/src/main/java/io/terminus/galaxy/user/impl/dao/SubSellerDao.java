package io.terminus.galaxy.user.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.galaxy.user.model.SubSeller;
import org.springframework.stereotype.Repository;

/**
 * @author Effet
 */
@Repository
public class SubSellerDao extends MyBatisDao<SubSeller> {

    public SubSeller findByUserId(Long userId) {
        return getSqlSession().selectOne(sqlId("findByUserId"), userId);
    }

    public SubSeller findByShopIdAndUserId(Long shopId, Long userId) {
        return getSqlSession().selectOne(sqlId("findByShopIdAndUserId"),
                ImmutableMap.of("shopId", shopId, "userId", userId));
    }
}
