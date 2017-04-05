package io.terminus.parana.msg.impl.dao.mysql;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.msg.model.ReceiverGroup;
import org.springframework.stereotype.Repository;

@Repository
public class ReceiverGroupDao extends MyBatisDao {
   public ReceiverGroup findByUserId(Long userId) {
      return (ReceiverGroup)this.getSqlSession().selectOne(this.sqlId("findByUserId"), ImmutableMap.of("userId", userId));
   }
}
