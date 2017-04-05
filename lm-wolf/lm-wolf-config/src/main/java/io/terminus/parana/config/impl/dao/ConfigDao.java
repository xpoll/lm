package io.terminus.parana.config.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.config.model.Config;
import java.util.List;
import org.springframework.stereotype.Repository;

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
