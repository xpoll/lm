package cn.blmdz.wolf.msg.impl.dao.mysql;

import org.springframework.stereotype.Repository;

import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.wolf.msg.model.MessageTemplate;

@Repository
public class MessageTemplateDao extends MyBatisDao {
   public MessageTemplate findByName(String templateName) {
      return (MessageTemplate)this.getSqlSession().selectOne(this.sqlId("findByName"), ImmutableMap.of("name", templateName));
   }
}
