package io.terminus.parana.msg.impl.dao.mysql;

import com.google.common.base.Optional;
import io.terminus.common.model.Paging;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.msg.model.Notification;
import java.util.HashMap;
import java.util.Map;
import org.springframework.stereotype.Repository;

@Repository
public class NotificationDao extends MyBatisDao {
   public Optional getLastUnchecked(Long audienceId) {
      Map<String, Object> criteria = new HashMap();
      criteria.put("checked", Boolean.valueOf(false));
      criteria.put("audienceId", audienceId);
      criteria.put("limit", Integer.valueOf(1));
      criteria.put("offset", Integer.valueOf(0));
      Paging<Notification> page = this.paging(criteria);
      return page.getTotal().longValue() > 0L?Optional.of(page.getData().get(0)):Optional.absent();
   }

   public Long count(Map criteria) {
      return (Long)this.getSqlSession().selectOne(this.sqlId("count"), criteria);
   }
}
