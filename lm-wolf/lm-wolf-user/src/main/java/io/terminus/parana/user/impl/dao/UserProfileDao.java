package io.terminus.parana.user.impl.dao;

import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.user.model.UserProfile;
import org.springframework.stereotype.Repository;

@Repository
public class UserProfileDao extends MyBatisDao {
   public UserProfile findByUserId(Long userId) {
      return (UserProfile)this.getSqlSession().selectOne(this.sqlId("findByUserId"), userId);
   }

   public Boolean deleteByUserId(Long userId) {
      return Boolean.valueOf(this.getSqlSession().delete(this.sqlId("deleteByUserId"), userId) == 1);
   }
}
