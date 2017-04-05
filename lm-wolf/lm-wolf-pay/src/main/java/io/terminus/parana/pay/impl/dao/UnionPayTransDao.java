package io.terminus.parana.pay.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.pay.model.UnionPayTrans;
import java.util.List;
import org.springframework.stereotype.Repository;

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
