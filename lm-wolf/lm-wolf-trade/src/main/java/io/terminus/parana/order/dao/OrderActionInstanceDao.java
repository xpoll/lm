package io.terminus.parana.order.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class OrderActionInstanceDao extends MyBatisDao {
   public List findByNsidAndType(Long nodeInstanceId, Integer actionType) {
      return this.getSqlSession().selectList(this.sqlId("findByNsidAndType"), ImmutableMap.of("nodeInstanceId", nodeInstanceId, "actionType", actionType));
   }

   public List findByNsid(Long nodeInstanceId) {
      return this.getSqlSession().selectList(this.sqlId("findByNodeInstanceId"), nodeInstanceId);
   }
}
