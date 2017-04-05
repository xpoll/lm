package io.terminus.parana.msg.impl.dao.mysql;

import io.terminus.common.mysql.dao.MyBatisDao;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository
public class MessageBoxDao extends MyBatisDao {
   public List getAllCheckedNoticationIds(Long userId) {
      return this.getSqlSession().selectList(this.sqlId("findNotificationByUserId"), userId);
   }
}
