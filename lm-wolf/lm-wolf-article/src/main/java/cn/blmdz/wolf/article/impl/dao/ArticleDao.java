package cn.blmdz.wolf.article.impl.dao;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;

@Repository
public class ArticleDao extends MyBatisDao {
   public List findByType(Integer type) {
      return this.getSqlSession().selectList(this.sqlId("findByType"), type);
   }

   public Integer setStatus(Long id, Integer status) {
      return Integer.valueOf(this.getSqlSession().update(this.sqlId("setStatus"), ImmutableMap.of("id", id, "status", status)));
   }
}
