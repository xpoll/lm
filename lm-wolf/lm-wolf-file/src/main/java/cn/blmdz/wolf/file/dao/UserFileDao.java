package cn.blmdz.wolf.file.dao;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.file.model.UserFile;

@Repository
public class UserFileDao extends MyBatisDao<UserFile> {
   public Boolean deleteByFolderId(Long folderId) {
      return Boolean.valueOf(this.getSqlSession().delete(this.sqlId("deleteByFolderId"), ImmutableMap.of("folderId", folderId)) >= 0);
   }
}
