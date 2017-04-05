package io.terminus.parana.user.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.user.model.UserDevice;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class UserDeviceDao extends MyBatisDao {
   public List findByUserId(Long userId) {
      return this.getSqlSession().selectList(this.sqlId("findByUserId"), userId);
   }

   public UserDevice findByDeviceToken(String deviceToken) {
      return (UserDevice)this.getSqlSession().selectOne(this.sqlId("findByDeviceToken"), ImmutableMap.of("deviceToken", deviceToken));
   }

   public Integer deleteByDeviceToken(String deviceToken) {
      return Integer.valueOf(this.getSqlSession().delete(this.sqlId("deleteByDeviceToken"), ImmutableMap.of("deviceToken", deviceToken)));
   }

   public Integer deleteByUserIdAndDeviceType(Long userId, String deviceType) {
      return Integer.valueOf(this.getSqlSession().delete(this.sqlId("deleteByUserIdAndDeviceType"), ImmutableMap.of("userId", userId, "deviceType", deviceType)));
   }
}
