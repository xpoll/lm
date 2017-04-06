package io.terminus.galaxy.item.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.item.model.Item;
import org.springframework.stereotype.Repository;

import java.util.Map;

/**
 * Date: 7/19/16
 * Time: 11:45 AM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
@Repository
public class GalaxyItemDao extends MyBatisDao<Item> {
    public Long count(Map<String, Object> criteria) {
        return getSqlSession().selectOne(sqlId("count"), criteria);
    }
}
