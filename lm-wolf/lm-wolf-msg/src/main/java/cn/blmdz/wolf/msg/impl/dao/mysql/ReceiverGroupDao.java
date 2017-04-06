package cn.blmdz.wolf.msg.impl.dao.mysql;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.msg.model.ReceiverGroup;

@Repository
public class ReceiverGroupDao extends MyBatisDao {
   public ReceiverGroup findByUserId(Long userId) {
      return (ReceiverGroup)this.getSqlSession().selectOne(this.sqlId("findByUserId"), ImmutableMap.of("userId", userId));
   }
}
