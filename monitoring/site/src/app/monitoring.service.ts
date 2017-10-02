import { Injectable } from '@angular/core';
import { Http } from '@angular/http';
import { Observable, Subscriber } from 'rxjs/Rx';

import {
  Overview,
  CPU,
  Memory,
  Network,
} from './monitoring';

@Injectable()
export class MonitoringService {
  private source: any;

  constructor(private http: Http) {
    if (window['source'] != undefined) {
      this.source = window['source']
    }
  }

  getOverview(): Observable<Overview> {
    return this.getObservable<Overview>('overview');
  }

  getCPU(): Observable<CPU> {
    return this.getObservable<CPU>('cpu');
  }

  getMemory(): Observable<Memory> {
    return this.getObservable<Memory>('mem');
  }

  getNetwork(): Observable<Network> {
    return this.getObservable<Network>('net');
  }

  removeEventListener(name: string) {
    this.source.removeEventListener(name);
  }

  private eventHandler(msg: any) {

  }

  private getObservable<T>(name: string): Observable<T> {
    if (this.source == undefined) {
      // TODO : EventSource non supportée
    }

    return Observable.create(
      (sub: Subscriber<T>) => {
        this.source.addEventListener(name,
          function(msg: any) {
            sub.next(JSON.parse(msg.data));
          });
      });
  }
}
