package io.terminus.parana.pay.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.pay.model.OwnerPayChannel;
import org.springframework.stereotype.Repository;

@Repository
public class OwnerPayChannelDao extends MyBatisDao {
   public OwnerPayChannel findByOwnerIdAndType(Long ownerId, Integer type) {
      return (OwnerPayChannel)this.getSqlSession().selectOne(this.sqlId("findByOwnerIdAndType"), ImmutableMap.of("ownerId", ownerId, "type", type));
   }
}
