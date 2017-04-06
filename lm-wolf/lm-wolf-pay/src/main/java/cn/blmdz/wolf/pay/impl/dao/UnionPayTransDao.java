package cn.blmdz.wolf.pay.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.pay.model.UnionPayTrans;

@Repository
public class UnionPayTransDao extends MyBatisDao {
   public UnionPayTrans findByQueryId(String queryId) {
      return (UnionPayTrans)this.getSqlSession().selectOne(this.sqlId("findByQueryId"), queryId);
   }

   public UnionPayTrans findForwardByQueryId(String queryId) {
      return (UnionPayTrans)this.getSqlSession().selectOne(this.sqlId("findForwardByQueryId"), queryId);
   }

   public List findReverseByQueryId(String queryId) {
      return this.getSqlSession().selectList(this.sqlId("findReverseByQueryId"), queryId);
   }
}
