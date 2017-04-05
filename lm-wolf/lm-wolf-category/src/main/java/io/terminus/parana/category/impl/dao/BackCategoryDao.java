package io.terminus.parana.category.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.category.model.BackCategory;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class BackCategoryDao extends MyBatisDao {
   public List findChildren(Long pid) {
      return this.getSqlSession().selectList(this.sqlId("findByPid"), pid);
   }

   public BackCategory findChildrenByName(Long pid, String name) {
      return (BackCategory)this.getSqlSession().selectOne(this.sqlId("findByPidAndName"), ImmutableMap.of("pid", pid, "name", name));
   }

   public boolean updateStatusById(Long id, Integer status) {
      return this.getSqlSession().update(this.sqlId("updateStatus"), ImmutableMap.of("id", id, "status", status)) == 1;
   }
}
