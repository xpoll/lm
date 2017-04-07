package cn.blmdz.rabbit.user.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.rabbit.user.model.OperatorRole;

/**
 * @author Effet
 */
@Repository
public class OperatorRoleDao extends MyBatisDao<OperatorRole> {

    public List<OperatorRole> findByStatus(String appKey, Integer status) {
        return getSqlSession().selectList(sqlId("findByStatus"),
                ImmutableMap.of("appKey", appKey, "status", status));
    }
}
