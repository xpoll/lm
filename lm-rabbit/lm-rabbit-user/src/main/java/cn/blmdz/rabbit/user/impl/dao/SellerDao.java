package cn.blmdz.rabbit.user.impl.dao;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.rabbit.user.model.Seller;

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
