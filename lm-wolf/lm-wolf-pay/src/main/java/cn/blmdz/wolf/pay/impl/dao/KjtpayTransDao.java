package cn.blmdz.wolf.pay.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.pay.model.KjtpayTrans;

@Repository
public class KjtpayTransDao extends MyBatisDao {
   public KjtpayTrans findByOuterNo(String outerNo) {
      return (KjtpayTrans)this.getSqlSession().selectOne(this.sqlId("findByOuterNo"), outerNo);
   }

   public List findByOrigOuterNo(String origOuterNo) {
      return this.getSqlSession().selectList(this.sqlId("findByOrigOuterNo"), origOuterNo);
   }

   public KjtpayTrans findByInnerNo(String innerNo) {
      return (KjtpayTrans)this.getSqlSession().selectOne(this.sqlId("findByInnerNo"), innerNo);
   }

   public KjtpayTrans findRefundTrans(String batchNo) {
      return (KjtpayTrans)this.getSqlSession().selectOne(this.sqlId("findRefundTrans"), batchNo);
   }
}
