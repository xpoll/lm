package io.terminus.galaxy.user.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.galaxy.user.model.MainSellerRole;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * @author Effet
 */
@Repository
public class MainSellerRoleDao extends MyBatisDao<MainSellerRole> {

    public List<MainSellerRole> findByStatus(Integer status) {
        return getSqlSession().selectList(sqlId("findByStatus"), status);
    }
}
