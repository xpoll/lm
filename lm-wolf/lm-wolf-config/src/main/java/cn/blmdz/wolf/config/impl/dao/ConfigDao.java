package cn.blmdz.wolf.config.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.config.model.Config;

@Repository
public class ConfigDao extends MyBatisDao {
   public List findByKey(String key) {
      return this.getSqlSession().selectList(this.sqlId("findByKey"), key);
   }

   public List findByBizType(int bizType) {
      return this.getSqlSession().selectList(this.sqlId("findByBizType"), Integer.valueOf(bizType));
   }

   public List findByGroup(String group) {
      return this.getSqlSession().selectList(this.sqlId("findByGroup"), group);
   }

   public Config findByUniqueIndex(int bizType, String key) {
      return (Config)this.getSqlSession().selectOne(this.sqlId("findByUniqueIndex"), ImmutableMap.of("bizType", Integer.valueOf(bizType), "key", key));
   }
}
