package cn.blmdz.wolf.pay.impl.dao;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.pay.model.OwnerPayChannel;

@Repository
public class OwnerPayChannelDao extends MyBatisDao {
   public OwnerPayChannel findByOwnerIdAndType(Long ownerId, Integer type) {
      return (OwnerPayChannel)this.getSqlSession().selectOne(this.sqlId("findByOwnerIdAndType"), ImmutableMap.of("ownerId", ownerId, "type", type));
   }
}
