package cn.blmdz.wolf.msg.impl.dao.mysql;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.msg.model.Subscription;

@Repository
public class SubscriptionDao extends MyBatisDao {
   public Subscription findByAccount(String account) {
      return (Subscription)this.getSqlSession().selectOne(this.sqlId("findByAccount"), ImmutableMap.of("account", account));
   }

   public List findByUserId(Long userId) {
      return this.getSqlSession().selectList(this.sqlId("findByUserId"), ImmutableMap.of("userId", userId));
   }
}
