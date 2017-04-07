package cn.blmdz.rabbit.user.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.rabbit.user.model.MainSellerRole;

/**
 * @author Effet
 */
@Repository
public class MainSellerRoleDao extends MyBatisDao<MainSellerRole> {

    public List<MainSellerRole> findByStatus(Integer status) {
        return getSqlSession().selectList(sqlId("findByStatus"), status);
    }
}
