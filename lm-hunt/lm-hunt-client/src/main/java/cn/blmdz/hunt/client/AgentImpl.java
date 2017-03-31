package cn.blmdz.hunt.client;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import com.alibaba.dubbo.config.spring.ServiceBean;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.InnerCookie;
import cn.blmdz.hunt.protocol.Action;
import cn.blmdz.hunt.protocol.Export;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class AgentImpl implements Agent, InitializingBean, ApplicationContextAware {
   private ApplicationContext applicationContext;
   private Map<String, ParamUtil.MethodInfo> methodInfoMap = Maps.newHashMap();

   public void setApplicationContext(ApplicationContext applicationContext) {
      this.applicationContext = applicationContext;
   }

   public void afterPropertiesSet() throws Exception {
      Map<String, ServiceBean> serviceBeans = this.applicationContext.getBeansOfType(ServiceBean.class);

      for(ServiceBean serviceBean : serviceBeans.values()) {
         Object bean = serviceBean.getRef();
         Class interfaceClass = serviceBean.getInterfaceClass();
         log.debug("dubbo serviceBean found: {}", interfaceClass.getName());
         this.registerMethod(bean, interfaceClass);
      }

      Map<String, Object> annotationBeans = this.applicationContext.getBeansWithAnnotation(Export.class);

      for(Object bean : annotationBeans.values()) {
         Class clazz = bean.getClass();
         log.debug("bean with @Export found: {}", clazz.getName());
         this.registerMethod(bean, clazz);
      }

   }

   private void registerMethod(Object bean, Class clazz) {
      for(Method method : clazz.getDeclaredMethods()) {
         ParamUtil.MethodInfo methodInfo = ParamUtil.getMethodInfo(bean, method);
         if(methodInfo != null) {
            String key = clazz.getName() + ":" + method.getName();
            this.methodInfoMap.put(key, methodInfo);
            log.debug("method exported: {}", key);
         }
      }

   }

   public LinkedHashMap getParamsInfo(String key) {
      ParamUtil.MethodInfo methodInfo = (ParamUtil.MethodInfo)this.methodInfoMap.get(key);
      if(methodInfo == null) {
         throw new NullPointerException("method not found: " + key);
      } else {
         LinkedHashMap<String, Agent.ParamInfo> paramsInfo = new LinkedHashMap();

         for(String paramKey : methodInfo.getParams().keySet()) {
            ParamUtil.ParamInfo richParamInfo = (ParamUtil.ParamInfo)methodInfo.getParams().get(paramKey);
            paramsInfo.put(paramKey, new Agent.ParamInfo(richParamInfo.isOptional(), richParamInfo.getClazz().getName()));
         }

         return paramsInfo;
      }
   }

   public WrapResp call(String key, Map params, Map context) {
      ParamUtil.MethodInfo methodInfo = (ParamUtil.MethodInfo)this.methodInfoMap.get(key);
      if(methodInfo == null) {
         throw new NullPointerException("method not found: " + key);
      } else {
         WrapResp resp = new WrapResp();
         Object[] args = new Object[methodInfo.getParams().size()];
         int index = 0;

         for(String paramName : methodInfo.getParams().keySet()) {
            ParamUtil.ParamInfo paramInfo = (ParamUtil.ParamInfo)methodInfo.getParams().get(paramName);
            Object arg = params.get(paramName);
            if(arg instanceof InnerCookie) {
               resp.setCookie((InnerCookie)arg);
            }

            args[index++] = ParamUtil.convert(arg, paramInfo, context);
         }

         Object result;
         try {
            result = methodInfo.getMethod().invoke(methodInfo.getBean(), args);
         } catch (InvocationTargetException | IllegalAccessException var12) {
            throw new RuntimeException("error when invoke method: " + key, var12);
         }

         if(result == null) {
            return resp;
         } else if(result instanceof Action) {
            Action action = (Action)result;
            action.setData(ParamUtil.convertResult(action.getData()));
            resp.setResult(action);
            return resp;
         } else if(result instanceof Response) {
            Response response = (Response)result;
            Response<Object> realResponse = new Response();
            if(response.isSuccess()) {
               realResponse.setResult(ParamUtil.convertResult(response.getResult()));
            } else {
               realResponse.setError(response.getError());
            }

            resp.setResult(realResponse);
            return resp;
         } else {
            resp.setResult(ParamUtil.convertResult(result));
            return resp;
         }
      }
   }
}
