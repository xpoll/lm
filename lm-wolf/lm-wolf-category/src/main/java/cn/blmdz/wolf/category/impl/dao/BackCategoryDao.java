package cn.blmdz.wolf.category.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.category.model.BackCategory;

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
