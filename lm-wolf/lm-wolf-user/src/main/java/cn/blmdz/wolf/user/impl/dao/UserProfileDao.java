package cn.blmdz.wolf.user.impl.dao;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.user.model.UserProfile;

@Repository
public class UserProfileDao extends MyBatisDao {
   public UserProfile findByUserId(Long userId) {
      return (UserProfile)this.getSqlSession().selectOne(this.sqlId("findByUserId"), userId);
   }

   public Boolean deleteByUserId(Long userId) {
      return Boolean.valueOf(this.getSqlSession().delete(this.sqlId("deleteByUserId"), userId) == 1);
   }
}
