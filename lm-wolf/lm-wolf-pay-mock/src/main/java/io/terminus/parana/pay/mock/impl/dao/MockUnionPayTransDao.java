package io.terminus.parana.pay.mock.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.pay.mock.model.MockUnionPayTrans;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class MockUnionPayTransDao extends MyBatisDao {
   public MockUnionPayTrans findByQueryId(String queryId) {
      return (MockUnionPayTrans)this.getSqlSession().selectOne(this.sqlId("findByQueryId"), queryId);
   }

   public MockUnionPayTrans findByOrderId(String queryId) {
      return (MockUnionPayTrans)this.getSqlSession().selectOne(this.sqlId("findByOrderId"), queryId);
   }

   public MockUnionPayTrans findForwardByQueryId(String queryId) {
      return (MockUnionPayTrans)this.getSqlSession().selectOne(this.sqlId("findForwardByQueryId"), queryId);
   }

   public List findReverseByQueryId(String queryId) {
      return this.getSqlSession().selectList(this.sqlId("findReverseByQueryId"), queryId);
   }
}
