package io.terminus.galaxy.user.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.galaxy.user.model.Seller;
import org.springframework.stereotype.Repository;

/**
 * @author Effet
 */
@Repository
public class SellerDao extends MyBatisDao<Seller> {

    public Seller findByUserId(Long userId) {
        return getSqlSession().selectOne(sqlId("findByUserId"), userId);
    }

    public Seller findByShopId(Long shopId) {
        return getSqlSession().selectOne(sqlId("findByShopId"), shopId);
    }
}
