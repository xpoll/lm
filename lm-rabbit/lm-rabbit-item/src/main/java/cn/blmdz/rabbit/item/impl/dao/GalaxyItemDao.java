package cn.blmdz.rabbit.item.impl.dao;

import java.util.Map;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.parana.item.model.Item;

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
