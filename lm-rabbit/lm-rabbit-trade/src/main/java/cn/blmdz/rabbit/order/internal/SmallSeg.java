package cn.blmdz.rabbit.order.internal;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * Author:cp
 * Created on 4/26/16.
 */
@Component
public class SmallSeg {
    @Autowired
    private Seg seg;

    public List<String> cut(String text) {
        return seg.cut(text);
    }
}