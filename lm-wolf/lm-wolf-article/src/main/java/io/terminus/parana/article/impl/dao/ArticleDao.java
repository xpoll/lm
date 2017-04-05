package io.terminus.parana.article.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class ArticleDao extends MyBatisDao {
   public List findByType(Integer type) {
      return this.getSqlSession().selectList(this.sqlId("findByType"), type);
   }

   public Integer setStatus(Long id, Integer status) {
      return Integer.valueOf(this.getSqlSession().update(this.sqlId("setStatus"), ImmutableMap.of("id", id, "status", status)));
   }
}
