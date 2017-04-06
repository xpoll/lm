package io.terminus.galaxy.user.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.galaxy.user.model.OperatorRole;
import org.springframework.stereotype.Repository;

import java.util.List;

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
