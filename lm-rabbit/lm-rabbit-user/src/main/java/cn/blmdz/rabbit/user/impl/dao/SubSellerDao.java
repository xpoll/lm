package cn.blmdz.rabbit.user.impl.dao;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.rabbit.user.model.SubSeller;

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
