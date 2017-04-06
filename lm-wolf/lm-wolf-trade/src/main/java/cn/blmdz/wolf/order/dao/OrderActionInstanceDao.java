package cn.blmdz.wolf.order.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;

@Repository
public class OrderActionInstanceDao extends MyBatisDao {
   public List findByNsidAndType(Long nodeInstanceId, Integer actionType) {
      return this.getSqlSession().selectList(this.sqlId("findByNsidAndType"), ImmutableMap.of("nodeInstanceId", nodeInstanceId, "actionType", actionType));
   }

   public List findByNsid(Long nodeInstanceId) {
      return this.getSqlSession().selectList(this.sqlId("findByNodeInstanceId"), nodeInstanceId);
   }
}
