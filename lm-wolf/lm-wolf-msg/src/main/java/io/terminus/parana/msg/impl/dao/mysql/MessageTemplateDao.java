package io.terminus.parana.msg.impl.dao.mysql;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.parana.msg.model.MessageTemplate;
import org.springframework.stereotype.Repository;

@Repository
public class MessageTemplateDao extends MyBatisDao {
   public MessageTemplate findByName(String templateName) {
      return (MessageTemplate)this.getSqlSession().selectOne(this.sqlId("findByName"), ImmutableMap.of("name", templateName));
   }
}
