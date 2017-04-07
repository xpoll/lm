/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package cn.blmdz.rabbit.web.core;

import java.util.concurrent.Executors;

import javax.servlet.Filter;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;
import org.springframework.web.filter.HiddenHttpMethodFilter;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.eventbus.AsyncEventBus;
import com.google.common.eventbus.EventBus;

import cn.blmdz.aide.file.FileServer;
import cn.blmdz.aide.file.ImageServer;
import cn.blmdz.aide.file.aliyun.AliyunFileServer;
import cn.blmdz.aide.file.aliyun.AliyunImageServer;
import cn.blmdz.aide.sms.SmsService;
import cn.blmdz.rabbit.web.core.component.MockSmsService;
import cn.blmdz.wolf.parana.search.item.ItemSearchWriteService;
import cn.blmdz.wolf.web.core.events.item.listener.IndexListener;

/**
 * Author:  <a href="mailto:i@terminus.io">jlchen</a>
 * Date: 2016-02-09
 */
@Configuration
@ComponentScan
public class GalaxyCoreWebConfiguration extends WebMvcConfigurerAdapter {

    @Bean
    public MessageSource messageSource() {
        ReloadableResourceBundleMessageSource messageSource = new ReloadableResourceBundleMessageSource();
        messageSource.setBasename("classpath:messages");
        messageSource.setCacheSeconds(3600);
        messageSource.setUseCodeAsDefaultMessage(true);
        messageSource.setDefaultEncoding("UTF-8");
        return messageSource;
    }

    @Bean
    public Filter hiddenHttpMethodFilter() {
        return new HiddenHttpMethodFilter();
    }

    @Bean(name = "imageServer")
    public ImageServer aliyunOSSImageServer(@Value("${oss.endpoint}")String endpoint,
                                            @Value("${oss.appKey}")String appKey,
                                            @Value("${oss.appSecret}")String appSecret,
                                            @Value("${oss.bucketName}")String bucketName){
        return new AliyunImageServer(endpoint,appKey, appSecret, bucketName);
    }

    @Bean(name = "fileServer")
    public FileServer aliyunOSSFileServer(@Value("${oss.endpoint}")String endpoint,
                                           @Value("${oss.appKey}")String appKey,
                                           @Value("${oss.appSecret}")String appSecret,
                                           @Value("${oss.bucketName}")String bucketName){
        return new AliyunFileServer(endpoint,appKey, appSecret, bucketName);
    }

    @Bean
    public EventBus eventBus(){
        return new AsyncEventBus(
                Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors()));
    }

    @Bean
    public IndexListener indexListener(ItemSearchWriteService itemSearchWriteService,
                                       EventBus eventBus){
        return new IndexListener(itemSearchWriteService, eventBus);
    }

    @Bean
    public ObjectMapper nonNullObjectMapper(){
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        return objectMapper;
    }

    @Bean
    @ConditionalOnProperty(name="msg.current.smsService", havingValue = "mockSmsService")
    public SmsService mockSmsService(){
        return new MockSmsService();
    }
}
