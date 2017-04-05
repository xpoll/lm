package io.terminus.parana.category.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class FrontCategoryDao extends MyBatisDao {
   public List findChildren(Long pid) {
      return this.getSqlSession().selectList(this.sqlId("findByPid"), pid);
   }

   public boolean updateHasChildren(Long id, Boolean hasChildren) {
      return this.getSqlSession().update(this.sqlId("updateHasChildren"), ImmutableMap.of("id", id, "hasChildren", hasChildren)) == 1;
   }
}
