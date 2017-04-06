package cn.blmdz.wolf.category.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;

@Repository
public class FrontCategoryDao extends MyBatisDao {
   public List findChildren(Long pid) {
      return this.getSqlSession().selectList(this.sqlId("findByPid"), pid);
   }

   public boolean updateHasChildren(Long id, Boolean hasChildren) {
      return this.getSqlSession().update(this.sqlId("updateHasChildren"), ImmutableMap.of("id", id, "hasChildren", hasChildren)) == 1;
   }
}
