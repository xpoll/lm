package io.terminus.parana.pay.mock.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.pay.mock.model.MockKjtpayTrans;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class MockKjtpayTransDao extends MyBatisDao {
   public MockKjtpayTrans findByOuterNo(String outerNo) {
      return (MockKjtpayTrans)this.getSqlSession().selectOne(this.sqlId("findByOuterNo"), outerNo);
   }

   public List findByOrigOuterNo(String origOuterNo) {
      return this.getSqlSession().selectList(this.sqlId("findByOrigOuterNo"), origOuterNo);
   }

   public MockKjtpayTrans findByInnerNo(String innerNo) {
      return (MockKjtpayTrans)this.getSqlSession().selectOne(this.sqlId("findByInnerNo"), innerNo);
   }

   public MockKjtpayTrans findRefundTrans(String batchNo) {
      return (MockKjtpayTrans)this.getSqlSession().selectOne(this.sqlId("findRefundTrans"), batchNo);
   }
}
