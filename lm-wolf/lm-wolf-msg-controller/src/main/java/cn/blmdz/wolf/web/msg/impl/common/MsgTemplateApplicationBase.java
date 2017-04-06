package cn.blmdz.wolf.web.msg.impl.common;

import java.io.IOException;
import java.io.Serializable;
import java.net.URLEncoder;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;
import com.github.jknack.handlebars.Template;
import com.google.common.base.Strings;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.home.common.util.MapBuilder;
import cn.blmdz.home.common.util.NumberUtils;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.wolf.common.util.ExUtil;
import cn.blmdz.wolf.msg.dto.AppPushReceivers;
import cn.blmdz.wolf.msg.dto.MessageInfo;
import cn.blmdz.wolf.msg.model.Channel;
import cn.blmdz.wolf.msg.model.Message;
import cn.blmdz.wolf.web.msg.MsgGatewayBuilder;
import cn.blmdz.wolf.web.msg.MsgTemplateApplication;

public abstract class MsgTemplateApplicationBase implements MsgTemplateApplication {
   private static final Logger log = LoggerFactory.getLogger(MsgTemplateApplicationBase.class);
   protected final MsgGatewayBuilder msgGatewayBuilder;
   protected static final Handlebars handlebars = new Handlebars();
   protected static final DecimalFormat DECIMAL_FORMAT = new DecimalFormat("#.###");

   public MsgTemplateApplicationBase(MsgGatewayBuilder msgGatewayBuilder) {
      this.msgGatewayBuilder = msgGatewayBuilder;
   }

   public List applyTemplate(Message message) {
      try {
         List<Message> messageList = new ArrayList();
         if(Objects.equals(Integer.valueOf(Channel.AppPush.value()), message.getChannel())) {
            AppPushReceivers receivers = (AppPushReceivers)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getReceivers(), AppPushReceivers.class);
            if(receivers.getAndroid() != null && receivers.getAndroid().size() > 0) {
               AppPushReceivers appPushReceivers = new AppPushReceivers();
               appPushReceivers.setAndroid(receivers.getAndroid());
               String androidReceivers = JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(appPushReceivers);
               Message androidMessage = this.doApplyTemplate(message, "android", androidReceivers);
               messageList.add(androidMessage);
            }

            if(receivers.getIos() != null && receivers.getIos().size() > 0) {
               AppPushReceivers appPushReceivers = new AppPushReceivers();
               appPushReceivers.setIos(receivers.getIos());
               String iosReceivers = JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(appPushReceivers);
               Message iosMessage = this.doApplyTemplate(message, "ios", iosReceivers);
               messageList.add(iosMessage);
            }

            if(receivers.getWp() != null && receivers.getWp().size() > 0) {
               AppPushReceivers appPushReceivers = new AppPushReceivers();
               appPushReceivers.setWp(receivers.getWp());
               String wpReceivers = JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(appPushReceivers);
               Message wpMessage = this.doApplyTemplate(message, "wp", wpReceivers);
               messageList.add(wpMessage);
            }
         } else {
            Message messageCopy = this.doApplyTemplate(message, (String)null, (String)null);
            messageList.add(messageCopy);
         }

         return messageList;
      } catch (Exception var7) {
         throw ExUtil.logJsonEx(var7, "applyTemplate", new Object[]{message});
      }
   }

   protected Message doApplyTemplate(Message message, String deviceType, String actualReceivers) {
      Message messageCopy = (Message)BeanMapper.map(message, Message.class);
      MessageInfo messageInfo = this.getMessageInfo(message, deviceType);
      messageCopy.setTitle(messageInfo.getMessageTitle());
      messageCopy.setContent(messageInfo.getMessageContent());
      if(!Strings.isNullOrEmpty(actualReceivers)) {
         messageCopy.setReceivers(actualReceivers);
      }

      return messageCopy;
   }

   protected MessageInfo getMessageInfo(Message message, String deviceType) {
      if(Strings.isNullOrEmpty(message.getTemplate())) {
         MessageInfo messageInfo = new MessageInfo();
         messageInfo.setMessageTitle(message.getTitle());
         messageInfo.setMessageContent(message.getContent());
         return messageInfo;
      } else {
         Map<String, Serializable> context = (Map)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message.getData(), Map.class);
         if(context == null) {
            context = new HashMap();
         }

         Template titleTemplate = this.getTitleTemplate(message.getTemplate(), message.getChannel(), deviceType);
         Template contentTemplate = this.getContentTemplate(message.getTemplate(), message.getChannel(), deviceType);
         if(contentTemplate == null) {
            log.error("content template not exists, message={}, deviceType={}", message, deviceType);
            throw new JsonResponseException("content.template.not.exists");
         } else {
            try {
               String title;
               if(titleTemplate != null) {
                  title = titleTemplate.apply(context);
               } else {
                  title = "message";
               }

               String content = contentTemplate.apply(context);
               MessageInfo messageInfo = new MessageInfo();
               messageInfo.setMessageTitle(title);
               messageInfo.setMessageContent(content);
               return messageInfo;
            } catch (IOException var9) {
               throw new JsonResponseException("apply.template.failed");
            }
         }
      }
   }

   protected String getActualTemplateName(String templateName, Integer channel, String deviceType) {
      String actualTemplateName;
      if(channel.equals(Integer.valueOf(Channel.Email.value()))) {
         actualTemplateName = templateName + "." + this.msgGatewayBuilder.getCurrentEmailService();
      } else if(channel.equals(Integer.valueOf(Channel.Sms.value()))) {
         actualTemplateName = templateName + "." + this.msgGatewayBuilder.getCurrentSmsService();
      } else {
         if(channel.equals(Integer.valueOf(Channel.Notification.value()))) {
            return templateName;
         }

         if(!channel.equals(Integer.valueOf(Channel.AppPush.value()))) {
            throw new JsonResponseException("channel.not.supported");
         }

         actualTemplateName = templateName + "." + deviceType;
      }

      return actualTemplateName;
   }

   protected abstract Template getTitleTemplate(String var1, Integer var2, String var3);

   protected abstract Template getContentTemplate(String var1, Integer var2, String var3);

   static {
      handlebars.registerHelper("assign", new Helper<String>() {
         public CharSequence apply(String name, Options options) throws IOException {
            CharSequence finalValue = options.apply(options.fn);
            options.context.data(name, finalValue.toString().trim());
            return null;
         }
      });
      handlebars.registerHelper("json", new Helper<Object>() {
         public CharSequence apply(Object context, Options options) throws IOException {
            return JsonMapper.nonEmptyMapper().toJson(context);
         }
      });
      handlebars.registerHelper("match", new Helper<String>() {
         public CharSequence apply(String regEx, Options options) throws IOException {
            Pattern pat = Pattern.compile(regEx);
            Matcher mat = pat.matcher((String)options.param(0));
            return mat.find()?options.fn():options.inverse();
         }
      });
      handlebars.registerHelper("gt", new Helper<Object>() {
         public CharSequence apply(Object source, Options options) throws IOException {
            long _source;
            if(source instanceof Long) {
               _source = ((Long)source).longValue();
            } else if(source instanceof Integer) {
               _source = (long)((Integer)source).intValue();
            } else {
               _source = Long.parseLong((String)source);
            }

            return _source > (long)((Integer)options.param(0)).intValue()?options.fn():options.inverse();
         }
      });
      handlebars.registerHelper("mod", new Helper<Integer>() {
         public CharSequence apply(Integer source, Options options) throws IOException {
            return (source.intValue() + 1) % ((Integer)options.param(0)).intValue() == 0?options.fn():options.inverse();
         }
      });
      handlebars.registerHelper("size", new Helper<Object>() {
         public CharSequence apply(Object context, Options options) throws IOException {
            return context == null?"0":(context instanceof Collection?String.valueOf(((Collection)context).size()):(context instanceof Map?String.valueOf(((Map)context).size()):"0"));
         }
      });
      handlebars.registerHelper("equals", new Helper<Object>() {
         public CharSequence apply(Object source, Options options) throws IOException {
            return com.google.common.base.Objects.equal(String.valueOf(source), String.valueOf(options.param(0)))?options.fn():options.inverse();
         }
      });
      handlebars.registerHelper("formatDate", new Helper<Object>() {
         Map sdfMap = MapBuilder.of().put("gmt", new SimpleDateFormat("EEE MMM dd HH:mm:ss Z yyyy"), "day", new SimpleDateFormat("yyyy-MM-dd"), "default", new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")).map();

         public CharSequence apply(Object dateObj, Options options) throws IOException {
            if(dateObj == null) {
               return "";
            } else {
               Date date = (new DateTime(dateObj)).toDate();
               String format = (String)options.param(0, "default");
               if(format.equals("ut")) {
                  return Long.toString(date.getTime());
               } else {
                  if(!this.sdfMap.containsKey(format)) {
                     this.sdfMap.put(format, new SimpleDateFormat(format));
                  }

                  return ((SimpleDateFormat)this.sdfMap.get(format)).format(date);
               }
            }
         }
      });
      handlebars.registerHelper("formatPrice", new Helper<Number>() {
         public CharSequence apply(Number price, Options options) throws IOException {
            return NumberUtils.formatPrice(price);
         }
      });
      handlebars.registerHelper("innerStyle", new Helper() {
         public CharSequence apply(Object context, Options options) throws IOException {
            if(context == null) {
               return "";
            } else {
               StringBuilder ret = new StringBuilder();
               String[] styles = ((String)context).split(";");

               for(String style : styles) {
                  String key = style.split(":")[0];
                  if(key.endsWith("radius")) {
                     ret.append(style).append(";");
                  }
               }

               return ret;
            }
         }
      });
      handlebars.registerHelper("cIndex", new Helper<Integer>() {
         public CharSequence apply(Integer context, Options options) throws IOException {
            return "" + (char)(context.intValue() + 65);
         }
      });
      handlebars.registerHelper("formatRate", new Helper<Double>() {
         public CharSequence apply(Double rate, Options options) throws IOException {
            return rate == null?"":MsgTemplateApplicationBase.DECIMAL_FORMAT.format(rate.doubleValue() / 1000.0D);
         }
      });
      handlebars.registerHelper("formatIntegerRate", new Helper<Integer>() {
         public CharSequence apply(Integer rate, Options options) throws IOException {
            return rate == null?"":MsgTemplateApplicationBase.DECIMAL_FORMAT.format((double)rate.intValue() / 1000.0D);
         }
      });
      handlebars.registerHelper("of", new Helper() {
         public CharSequence apply(Object source, Options options) throws IOException {
            if(source == null) {
               return options.inverse();
            } else {
               String _source = source.toString();
               String param = (String)options.param(0);
               if(Strings.isNullOrEmpty(param)) {
                  return options.inverse();
               } else {
                  List<String> targets = Splitters.COMMA.splitToList(param);
                  return targets.contains(_source)?options.fn():options.inverse();
               }
            }
         }
      });
      handlebars.registerHelper("add", new Helper() {
         public CharSequence apply(Object source, Options options) throws IOException {
            Object param = options.param(0);
            if(source == null && param == null) {
               return "";
            } else if(source == null) {
               return param.toString();
            } else if(param == null) {
               return source.toString();
            } else if(source instanceof Double) {
               Double first = (Double)source;
               Double second = (Double)param;
               return String.valueOf(first.doubleValue() + second.doubleValue());
            } else if(source instanceof Integer) {
               Integer first = (Integer)source;
               Integer second = (Integer)param;
               return String.valueOf(first.intValue() + second.intValue());
            } else if(source instanceof Long) {
               Long first = (Long)source;
               Long second = (Long)param;
               return String.valueOf(first.longValue() + second.longValue());
            } else if(source instanceof String) {
               Integer first = Integer.valueOf(Integer.parseInt(source.toString()));
               Integer second = Integer.valueOf(Integer.parseInt(param.toString()));
               return String.valueOf(first.intValue() + second.intValue());
            } else {
               throw new IllegalStateException("incorrect.type");
            }
         }
      });
      handlebars.registerHelper("rget", new Helper() {
         private final Random random = new Random(System.currentTimeMillis());

         public CharSequence apply(Object context, Options options) throws IOException {
            List list;
            if(context instanceof List) {
               list = (List)context;
            } else {
               list = Splitters.COMMA.splitToList(String.valueOf(context));
            }

            return list.isEmpty()?null:list.get(this.random.nextInt(list.size())).toString();
         }
      });
      handlebars.registerHelper("urlEncode", new Helper() {
         public CharSequence apply(Object param, Options options) throws IOException {
            String charset = (String)options.param(0, "utf-8");
            return URLEncoder.encode(param.toString(), charset);
         }
      });
   }
}
