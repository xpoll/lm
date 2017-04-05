package io.terminus.parana.file.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import org.springframework.stereotype.Repository;

@Repository
public class UserFileDao extends MyBatisDao {
   public Boolean deleteByFolderId(Long folderId) {
      return Boolean.valueOf(this.getSqlSession().delete(this.sqlId("deleteByFolderId"), ImmutableMap.of("folderId", folderId)) >= 0);
   }
}
