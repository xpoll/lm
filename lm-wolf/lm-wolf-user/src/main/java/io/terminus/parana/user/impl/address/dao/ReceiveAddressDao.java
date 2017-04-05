package io.terminus.parana.user.impl.address.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class ReceiveAddressDao extends MyBatisDao {
   public List findAddressByUserId(Long userId) {
      return this.getSqlSession().selectList(this.sqlId("findAddressByUserId"), userId);
   }

   public void makeDefault(Long addressId, Long userId) {
      this.getSqlSession().update(this.sqlId("makeDefault"), ImmutableMap.of("addressId", addressId, "userId", userId));
   }

   public void deleteAddressByAddressIdAndUserId(Long addressId, Long userId) {
      this.getSqlSession().update(this.sqlId("deleteByAddressIdAndUserId"), ImmutableMap.of("addressId", addressId, "userId", userId));
   }
}
