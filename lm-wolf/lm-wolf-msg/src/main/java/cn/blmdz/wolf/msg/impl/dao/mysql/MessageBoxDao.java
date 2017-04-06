package cn.blmdz.wolf.msg.impl.dao.mysql;

import java.util.List;

import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;

@Repository
public class MessageBoxDao extends MyBatisDao {
   public List getAllCheckedNoticationIds(Long userId) {
      return this.getSqlSession().selectList(this.sqlId("findNotificationByUserId"), userId);
   }
}
