package io.terminus.parana.msg.impl.dao.mysql;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.msg.model.Subscription;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class SubscriptionDao extends MyBatisDao {
   public Subscription findByAccount(String account) {
      return (Subscription)this.getSqlSession().selectOne(this.sqlId("findByAccount"), ImmutableMap.of("account", account));
   }

   public List findByUserId(Long userId) {
      return this.getSqlSession().selectList(this.sqlId("findByUserId"), ImmutableMap.of("userId", userId));
   }
}
