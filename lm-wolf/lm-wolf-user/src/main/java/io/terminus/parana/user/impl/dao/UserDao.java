package io.terminus.parana.user.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.user.model.User;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Repository;

@Repository
public class UserDao extends MyBatisDao {
   public User findByName(String name) {
      return (User)this.getSqlSession().selectOne(this.sqlId("findByName"), name);
   }

   public User findByEmail(String email) {
      return (User)this.getSqlSession().selectOne(this.sqlId("findByEmail"), email);
   }

   public User findByMobile(String mobile) {
      return (User)this.getSqlSession().selectOne(this.sqlId("findByMobile"), mobile);
   }

   public Integer updateStatus(Long userId, Integer status) {
      return Integer.valueOf(this.getSqlSession().update(this.sqlId("updateStatus"), ImmutableMap.of("id", userId, "status", status)));
   }

   public Integer batchUpdateStatus(List userIds, Integer status) {
      return Integer.valueOf(this.getSqlSession().update(this.sqlId("batchUpdateStatus"), ImmutableMap.of("ids", userIds, "status", status)));
   }

   public Integer updateType(Long userId, int type) {
      return Integer.valueOf(this.getSqlSession().update(this.sqlId("updateType"), ImmutableMap.of("userId", userId, "type", Integer.valueOf(type))));
   }

   public void updateTags(Long userId, Map tags) {
      this.getSqlSession().update(this.sqlId("updateTags"), ImmutableMap.of("userId", userId, "tagsJson", JsonMapper.JSON_NON_DEFAULT_MAPPER.toJson(tags)));
   }

   public void updateRoles(Long userId, List roles) {
      this.getSqlSession().update(this.sqlId("updateRoles"), ImmutableMap.of("userId", userId, "rolesJson", JsonMapper.JSON_NON_DEFAULT_MAPPER.toJson(roles)));
   }
}
